Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zcurrentgstate.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zcurrentgstate_z := 1%positive.
Notation V_zcurrentgstate__tmp := 2%positive.
Notation V_zcurrentgstate_code := 3%positive.
Notation V_zcurrentgstate_i := 4%positive.
Notation V_zcurrentgstate_i2 := 5%positive.
Notation V_zcurrentgstate_op := 6%positive.
Definition Pedges_zcurrentgstate: list (edge proc) :=
  (EA 1 (AAssign V_zcurrentgstate_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 4)::(EA 3 ANone 6)::(EA 4 AWeaken 5)::(EA 5 ANone 9)::
  (EA 5 ANone 6)::(EA 6 (AAssign V_zcurrentgstate__tmp None) 7)::
  (EA 7 ANone 8)::(EA 8 AWeaken 66)::(EA 9 AWeaken 10)::(EA 10 ANone 14)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_zcurrentgstate__tmp
  (Some (ENum (-7)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 66)::
  (EA 14 (AAssign V_zcurrentgstate_code None) 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_zcurrentgstate_code) s) <
  (eval (ENum (0)) s))%Z)) 62)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_zcurrentgstate_code) s) >= (eval (ENum (0))
  s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign V_zcurrentgstate_code
  None) 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_zcurrentgstate_code) s) < (eval (ENum (0))
  s))%Z)) 58)::(EA 20 (AGuard (fun s => ((eval (EVar V_zcurrentgstate_code)
  s) >= (eval (ENum (0)) s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_zcurrentgstate_i (Some (ENum (25)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 ANone 27)::(EA 25 ANone 26)::(EA 26 ANone 28)::
  (EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign V_zcurrentgstate_i
  (Some (EAdd (EVar V_zcurrentgstate_i) (ENum (-1))))) 30)::
  (EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcurrentgstate_i) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 54)::(EA 31 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcurrentgstate_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 (AAssign
  V_zcurrentgstate_code None) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_zcurrentgstate_code) s) < (eval (ENum (0))
  s))%Z)) 50)::(EA 35 (AGuard (fun s => ((eval (EVar V_zcurrentgstate_code)
  s) >= (eval (ENum (0)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_zcurrentgstate_i2 (Some (ENum (25)))) 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_zcurrentgstate_i2
  (Some (EAdd (EVar V_zcurrentgstate_i2) (ENum (-1))))) 41)::
  (EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcurrentgstate_i2) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 47)::(EA 42 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcurrentgstate_i2) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 43)::(EA 43 AWeaken 44)::(EA 44 (AAssign
  V_zcurrentgstate__tmp (Some (ENum (0)))) 45)::(EA 45 ANone 46)::
  (EA 46 AWeaken 66)::(EA 47 AWeaken 48)::(EA 48 ANone 49)::(EA 49 (AAssign
  V_zcurrentgstate_z (Some (EAdd (ENum (1))
  (EVar V_zcurrentgstate_z)))) 39)::(EA 50 AWeaken 51)::(EA 51 (AAssign
  V_zcurrentgstate__tmp (Some (EVar V_zcurrentgstate_code))) 52)::
  (EA 52 ANone 53)::(EA 53 AWeaken 66)::(EA 54 AWeaken 55)::
  (EA 55 ANone 56)::(EA 56 (AAssign V_zcurrentgstate_z (Some (EAdd (ENum (1))
  (EVar V_zcurrentgstate_z)))) 57)::(EA 57 AWeaken 25)::(EA 58 AWeaken 59)::
  (EA 59 (AAssign V_zcurrentgstate__tmp
  (Some (EVar V_zcurrentgstate_code))) 60)::(EA 60 ANone 61)::
  (EA 61 AWeaken 66)::(EA 62 AWeaken 63)::(EA 63 (AAssign
  V_zcurrentgstate__tmp (Some (EVar V_zcurrentgstate_code))) 64)::
  (EA 64 ANone 65)::(EA 65 AWeaken 66)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zcurrentgstate => Pedges_zcurrentgstate
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zcurrentgstate => 66
     end)%positive;
  var_global := var_global
}.

Definition ai_zcurrentgstate (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 3 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0)%Z
   | 4 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 5 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0)%Z
   | 6 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 7 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0)%Z
   | 8 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 9 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 10 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0)%Z
   | 11 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 12 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate__tmp + 7 <= 0 /\ -1 * s V_zcurrentgstate__tmp + -7 <= 0)%Z
   | 13 => (-1 * s V_zcurrentgstate__tmp + -7 <= 0 /\ 1 * s V_zcurrentgstate__tmp + 7 <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 14 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 15 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0)%Z
   | 16 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 17 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0)%Z
   | 18 => (-1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 19 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0)%Z
   | 20 => (1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 21 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0)%Z
   | 22 => (-1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 23 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_i + 25 <= 0)%Z
   | 24 => (-1 * s V_zcurrentgstate_i + 25 <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 25 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0)%Z
   | 26 => (-1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 27 => (-1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 28 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0)%Z
   | 29 => (-1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -25 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 30 => (-1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -24 <= 0)%Z
   | 31 => (1 * s V_zcurrentgstate_i + -24 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 32 => (-1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0)%Z
   | 33 => (-1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 34 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0)%Z
   | 35 => (-1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 36 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0)%Z
   | 37 => (-1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 38 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i2 + -25 <= 0 /\ -1 * s V_zcurrentgstate_i2 + 25 <= 0)%Z
   | 39 => (1 * s V_zcurrentgstate_i2 + -25 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0)%Z
   | 40 => (-1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i2 + -25 <= 0)%Z
   | 41 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i2 + -24 <= 0)%Z
   | 42 => (1 * s V_zcurrentgstate_i2 + -24 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 43 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i2 + -1 <= 0 /\ -1 * s V_zcurrentgstate_i2 + 1 <= 0)%Z
   | 44 => (-1 * s V_zcurrentgstate_i2 + 1 <= 0 /\ 1 * s V_zcurrentgstate_i2 + -1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 45 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i2 + -1 <= 0 /\ -1 * s V_zcurrentgstate_i2 + 1 <= 0 /\ 1 * s V_zcurrentgstate__tmp <= 0 /\ -1 * s V_zcurrentgstate__tmp <= 0)%Z
   | 46 => (-1 * s V_zcurrentgstate__tmp <= 0 /\ 1 * s V_zcurrentgstate__tmp <= 0 /\ -1 * s V_zcurrentgstate_i2 + 1 <= 0 /\ 1 * s V_zcurrentgstate_i2 + -1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 47 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i2 + -24 <= 0)%Z
   | 48 => (1 * s V_zcurrentgstate_i2 + -24 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 49 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i2 + -24 <= 0)%Z
   | 50 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0)%Z
   | 51 => (1 * s V_zcurrentgstate_code + 1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 52 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate__tmp + 1 <= 0)%Z
   | 53 => (1 * s V_zcurrentgstate__tmp + 1 <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0 /\ -1 * s V_zcurrentgstate_i + 1 <= 0 /\ 1 * s V_zcurrentgstate_i + -1 <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 54 => (-1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -24 <= 0)%Z
   | 55 => (1 * s V_zcurrentgstate_i + -24 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 56 => (-1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ 1 * s V_zcurrentgstate_i + -24 <= 0)%Z
   | 57 => (1 * s V_zcurrentgstate_i + -24 <= 0 /\ -1 * s V_zcurrentgstate_code <= 0 /\ -1 * s V_zcurrentgstate_z + 1 <= 0)%Z
   | 58 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0)%Z
   | 59 => (1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 60 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate__tmp + 1 <= 0)%Z
   | 61 => (1 * s V_zcurrentgstate__tmp + 1 <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 62 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0)%Z
   | 63 => (1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 64 => (-1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate__tmp + 1 <= 0)%Z
   | 65 => (1 * s V_zcurrentgstate__tmp + 1 <= 0 /\ 1 * s V_zcurrentgstate_code + 1 <= 0 /\ 1 * s V_zcurrentgstate_z <= 0 /\ -1 * s V_zcurrentgstate_z <= 0)%Z
   | 66 => (-1 * s V_zcurrentgstate_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zcurrentgstate (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((46 # 1) <= z)%Q
   | 2 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 3 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 4 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 5 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 6 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 7 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 8 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 9 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 10 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 11 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 12 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 13 => hints
     [(*0 46*) F_one]
     ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 14 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 15 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 16 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 17 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 18 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 19 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 20 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 21 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 22 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 23 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 24 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 25 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 26 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 27 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 28 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 29 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 30 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 31 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 32 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 33 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 34 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zcurrentgstate_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zcurrentgstate_i))]
     ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 35 => ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 36 => ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 37 => ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 38 => (-(2 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 39 => (-(2 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 40 => (-(2 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 41 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 42 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 43 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 44 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 45 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 46 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zcurrentgstate_i2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zcurrentgstate_i2) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zcurrentgstate_i2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zcurrentgstate_i)) (F_check_ge (0) (0))]
     (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
      + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 47 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 48 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 49 => (-(1 # 1) + s V_zcurrentgstate_i2 + s V_zcurrentgstate_z
            + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 50 => ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 51 => ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 52 => ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 53 => hints
     [(*-23 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zcurrentgstate_i)) (F_check_ge (0) (0))]
     ((23 # 1) + s V_zcurrentgstate_z + max0(-1 + s V_zcurrentgstate_i) <= z)%Q
   | 54 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 55 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 56 => ((22 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 57 => ((21 # 1) + s V_zcurrentgstate_i + s V_zcurrentgstate_z <= z)%Q
   | 58 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 59 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 60 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 61 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 62 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 63 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 64 => ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 65 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcurrentgstate_z <= z)%Q
   | 66 => (s V_zcurrentgstate_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zcurrentgstate =>
    [mkPA Q (fun n z s => ai_zcurrentgstate n s /\ annot0_zcurrentgstate n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zcurrentgstate (proc_start P_zcurrentgstate) s1 (proc_end P_zcurrentgstate) s2 ->
    (s2 V_zcurrentgstate_z <= (46 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zcurrentgstate.
Qed.
