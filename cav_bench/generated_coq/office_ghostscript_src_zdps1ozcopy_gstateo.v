Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zcopy_gstate.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zcopy_gstate_z := 1%positive.
Notation V_zcopy_gstate__tmp := 2%positive.
Notation V_zcopy_gstate_code := 3%positive.
Notation V_zcopy_gstate_i := 4%positive.
Notation V_zcopy_gstate_i2 := 5%positive.
Notation V_zcopy_gstate_op := 6%positive.
Definition Pedges_zcopy_gstate: list (edge proc) :=
  (EA 1 (AAssign V_zcopy_gstate_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 4)::(EA 3 ANone 6)::(EA 4 AWeaken 5)::(EA 5 ANone 9)::
  (EA 5 ANone 6)::(EA 6 (AAssign V_zcopy_gstate__tmp None) 7)::
  (EA 7 ANone 8)::(EA 8 AWeaken 73)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 10 ANone 13)::(EA 11 AWeaken 12)::(EA 12 ANone 16)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_zcopy_gstate__tmp None) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 73)::(EA 16 AWeaken 17)::(EA 17 ANone 21)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_zcopy_gstate__tmp
  (Some (ENum (-7)))) 19)::(EA 19 ANone 20)::(EA 20 AWeaken 73)::
  (EA 21 (AAssign V_zcopy_gstate_code None) 22)::(EA 22 AWeaken 23)::
  (EA 23 (AGuard (fun s => ((eval (EVar V_zcopy_gstate_code) s) <
  (eval (ENum (0)) s))%Z)) 69)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_zcopy_gstate_code) s) >= (eval (ENum (0))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 (AAssign V_zcopy_gstate_code
  None) 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_zcopy_gstate_code) s) < (eval (ENum (0))
  s))%Z)) 65)::(EA 27 (AGuard (fun s => ((eval (EVar V_zcopy_gstate_code)
  s) >= (eval (ENum (0)) s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_zcopy_gstate_i (Some (ENum (25)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 ANone 34)::(EA 32 ANone 33)::(EA 33 ANone 35)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign V_zcopy_gstate_i
  (Some (EAdd (EVar V_zcopy_gstate_i) (ENum (-1))))) 37)::
  (EA 37 AWeaken 38)::(EA 38 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcopy_gstate_i) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 61)::(EA 38 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcopy_gstate_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 39)::(EA 39 AWeaken 40)::(EA 40 (AAssign
  V_zcopy_gstate_code None) 41)::(EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_zcopy_gstate_code) s) < (eval (ENum (0))
  s))%Z)) 57)::(EA 42 (AGuard (fun s => ((eval (EVar V_zcopy_gstate_code)
  s) >= (eval (ENum (0)) s))%Z)) 43)::(EA 43 AWeaken 44)::(EA 44 (AAssign
  V_zcopy_gstate_i2 (Some (ENum (25)))) 45)::(EA 45 ANone 46)::
  (EA 46 ANone 47)::(EA 47 (AAssign V_zcopy_gstate_i2
  (Some (EAdd (EVar V_zcopy_gstate_i2) (ENum (-1))))) 48)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcopy_gstate_i2) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 54)::(EA 49 (AGuard
  (fun s => ((eval (EAdd (EVar V_zcopy_gstate_i2) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 (AAssign
  V_zcopy_gstate__tmp (Some (ENum (0)))) 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 73)::(EA 54 AWeaken 55)::(EA 55 ANone 56)::(EA 56 (AAssign
  V_zcopy_gstate_z (Some (EAdd (ENum (1)) (EVar V_zcopy_gstate_z)))) 46)::
  (EA 57 AWeaken 58)::(EA 58 (AAssign V_zcopy_gstate__tmp
  (Some (EVar V_zcopy_gstate_code))) 59)::(EA 59 ANone 60)::
  (EA 60 AWeaken 73)::(EA 61 AWeaken 62)::(EA 62 ANone 63)::(EA 63 (AAssign
  V_zcopy_gstate_z (Some (EAdd (ENum (1)) (EVar V_zcopy_gstate_z)))) 64)::
  (EA 64 AWeaken 32)::(EA 65 AWeaken 66)::(EA 66 (AAssign V_zcopy_gstate__tmp
  (Some (EVar V_zcopy_gstate_code))) 67)::(EA 67 ANone 68)::
  (EA 68 AWeaken 73)::(EA 69 AWeaken 70)::(EA 70 (AAssign V_zcopy_gstate__tmp
  (Some (EVar V_zcopy_gstate_code))) 71)::(EA 71 ANone 72)::
  (EA 72 AWeaken 73)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zcopy_gstate => Pedges_zcopy_gstate
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zcopy_gstate => 73
     end)%positive;
  var_global := var_global
}.

Definition ai_zcopy_gstate (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 3 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 4 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 5 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 6 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 7 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 8 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 9 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 10 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 11 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 12 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 13 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 14 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 15 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 16 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 17 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 18 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 19 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate__tmp + 7 <= 0 /\ -1 * s V_zcopy_gstate__tmp + -7 <= 0)%Z
   | 20 => (-1 * s V_zcopy_gstate__tmp + -7 <= 0 /\ 1 * s V_zcopy_gstate__tmp + 7 <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 21 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 22 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 23 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 24 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0)%Z
   | 25 => (-1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 26 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0)%Z
   | 27 => (1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 28 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0)%Z
   | 29 => (-1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 30 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_i + 25 <= 0)%Z
   | 31 => (-1 * s V_zcopy_gstate_i + 25 <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 32 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0)%Z
   | 33 => (-1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 34 => (-1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 35 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0)%Z
   | 36 => (-1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -25 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 37 => (-1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -24 <= 0)%Z
   | 38 => (1 * s V_zcopy_gstate_i + -24 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 39 => (-1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0)%Z
   | 40 => (-1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 41 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0)%Z
   | 42 => (-1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 43 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0)%Z
   | 44 => (-1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 45 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i2 + -25 <= 0 /\ -1 * s V_zcopy_gstate_i2 + 25 <= 0)%Z
   | 46 => (1 * s V_zcopy_gstate_i2 + -25 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0)%Z
   | 47 => (-1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i2 + -25 <= 0)%Z
   | 48 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i2 + -24 <= 0)%Z
   | 49 => (1 * s V_zcopy_gstate_i2 + -24 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 50 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i2 + -1 <= 0 /\ -1 * s V_zcopy_gstate_i2 + 1 <= 0)%Z
   | 51 => (-1 * s V_zcopy_gstate_i2 + 1 <= 0 /\ 1 * s V_zcopy_gstate_i2 + -1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 52 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i2 + -1 <= 0 /\ -1 * s V_zcopy_gstate_i2 + 1 <= 0 /\ 1 * s V_zcopy_gstate__tmp <= 0 /\ -1 * s V_zcopy_gstate__tmp <= 0)%Z
   | 53 => (-1 * s V_zcopy_gstate__tmp <= 0 /\ 1 * s V_zcopy_gstate__tmp <= 0 /\ -1 * s V_zcopy_gstate_i2 + 1 <= 0 /\ 1 * s V_zcopy_gstate_i2 + -1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 54 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i2 + -24 <= 0)%Z
   | 55 => (1 * s V_zcopy_gstate_i2 + -24 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 56 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i2 + -24 <= 0)%Z
   | 57 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0)%Z
   | 58 => (1 * s V_zcopy_gstate_code + 1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 59 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate__tmp + 1 <= 0)%Z
   | 60 => (1 * s V_zcopy_gstate__tmp + 1 <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0 /\ -1 * s V_zcopy_gstate_i + 1 <= 0 /\ 1 * s V_zcopy_gstate_i + -1 <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 61 => (-1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -24 <= 0)%Z
   | 62 => (1 * s V_zcopy_gstate_i + -24 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 63 => (-1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ 1 * s V_zcopy_gstate_i + -24 <= 0)%Z
   | 64 => (1 * s V_zcopy_gstate_i + -24 <= 0 /\ -1 * s V_zcopy_gstate_code <= 0 /\ -1 * s V_zcopy_gstate_z + 1 <= 0)%Z
   | 65 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0)%Z
   | 66 => (1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 67 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate__tmp + 1 <= 0)%Z
   | 68 => (1 * s V_zcopy_gstate__tmp + 1 <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 69 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0)%Z
   | 70 => (1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 71 => (-1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate__tmp + 1 <= 0)%Z
   | 72 => (1 * s V_zcopy_gstate__tmp + 1 <= 0 /\ 1 * s V_zcopy_gstate_code + 1 <= 0 /\ 1 * s V_zcopy_gstate_z <= 0 /\ -1 * s V_zcopy_gstate_z <= 0)%Z
   | 73 => (-1 * s V_zcopy_gstate_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zcopy_gstate (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((46 # 1) <= z)%Q
   | 2 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 3 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 4 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 5 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 6 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 7 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 8 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 9 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 10 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 11 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 12 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 13 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 14 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 15 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 16 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 17 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 18 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 19 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 20 => hints
     [(*0 46*) F_one]
     ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 21 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 22 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 23 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 24 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 25 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 26 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 27 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 28 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 29 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 30 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 31 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 32 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 33 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 34 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 35 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 36 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 37 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 38 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 39 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 40 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 41 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zcopy_gstate_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zcopy_gstate_i))]
     ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 42 => ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 43 => ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 44 => ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 45 => (-(2 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 46 => (-(2 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 47 => (-(2 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 48 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 49 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 50 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 51 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 52 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 53 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zcopy_gstate_i2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zcopy_gstate_i2) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zcopy_gstate_i2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zcopy_gstate_i)) (F_check_ge (0) (0))]
     (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
      + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 54 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 55 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 56 => (-(1 # 1) + s V_zcopy_gstate_i2 + s V_zcopy_gstate_z
            + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 57 => ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 58 => ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 59 => ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 60 => hints
     [(*-23 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zcopy_gstate_i)) (F_check_ge (0) (0))]
     ((23 # 1) + s V_zcopy_gstate_z + max0(-1 + s V_zcopy_gstate_i) <= z)%Q
   | 61 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 62 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 63 => ((22 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 64 => ((21 # 1) + s V_zcopy_gstate_i + s V_zcopy_gstate_z <= z)%Q
   | 65 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 66 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 67 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 68 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 69 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 70 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 71 => ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 72 => hints
     [(*-46 0*) F_one]
     ((46 # 1) + s V_zcopy_gstate_z <= z)%Q
   | 73 => (s V_zcopy_gstate_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zcopy_gstate =>
    [mkPA Q (fun n z s => ai_zcopy_gstate n s /\ annot0_zcopy_gstate n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zcopy_gstate (proc_start P_zcopy_gstate) s1 (proc_end P_zcopy_gstate) s2 ->
    (s2 V_zcopy_gstate_z <= (46 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zcopy_gstate.
Qed.
