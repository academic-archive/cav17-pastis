Require Import pasta.Pasta.

Inductive proc: Type :=
  P_inflate_block.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_inflate_block_z := 1%positive.
Notation V_inflate_block__tmp := 2%positive.
Notation V_inflate_block_b := 3%positive.
Notation V_inflate_block_bb := 4%positive.
Notation V_inflate_block_bk := 5%positive.
Notation V_inflate_block_e_dref := 6%positive.
Notation V_inflate_block_incnt := 7%positive.
Notation V_inflate_block_k := 8%positive.
Notation V_inflate_block_t := 9%positive.
Notation V_inflate_block_e := 10%positive.
Definition Pedges_inflate_block: list (edge proc) :=
  (EA 1 (AAssign V_inflate_block_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_inflate_block_k) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_inflate_block_b
  (Some (EVar V_inflate_block_bb))) 5)::(EA 5 (AAssign V_inflate_block_k
  (Some (EVar V_inflate_block_bk))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_inflate_block_k) s) <
  (eval (ENum (1)) s))%Z)) 60)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_inflate_block_k) s) >= (eval (ENum (1))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 (AAssign V_inflate_block_e_dref
  None) 11)::(EA 11 (AAssign V_inflate_block_b None) 12)::(EA 12 (AAssign
  V_inflate_block_k (Some (ESub (EVar V_inflate_block_k) (ENum (1))))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_inflate_block_k) s) < (eval (ENum (2))
  s))%Z)) 44)::(EA 15 (AGuard (fun s => ((eval (EVar V_inflate_block_k) s) >=
  (eval (ENum (2)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign
  V_inflate_block_t None) 18)::(EA 18 (AAssign V_inflate_block_b None) 19)::
  (EA 19 (AAssign V_inflate_block_k (Some (ESub (EVar V_inflate_block_k)
  (ENum (2))))) 20)::(EA 20 (AAssign V_inflate_block_bb
  (Some (EVar V_inflate_block_b))) 21)::(EA 21 (AAssign V_inflate_block_bk
  (Some (EVar V_inflate_block_k))) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_inflate_block_t) s) = (eval (ENum (2))
  s))%Z)) 40)::(EA 23 (AGuard (fun s => ((eval (EVar V_inflate_block_t) s) <>
  (eval (ENum (2)) s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_inflate_block_t) s) = (eval (ENum (0))
  s))%Z)) 36)::(EA 25 (AGuard (fun s => ((eval (EVar V_inflate_block_t) s) <>
  (eval (ENum (0)) s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_inflate_block_t) s) = (eval (ENum (1))
  s))%Z)) 32)::(EA 27 (AGuard (fun s => ((eval (EVar V_inflate_block_t) s) <>
  (eval (ENum (1)) s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_inflate_block__tmp (Some (ENum (2)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 76)::(EA 32 AWeaken 33)::(EA 33 (AAssign
  V_inflate_block__tmp None) 34)::(EA 34 ANone 35)::(EA 35 AWeaken 76)::
  (EA 36 AWeaken 37)::(EA 37 (AAssign V_inflate_block__tmp None) 38)::
  (EA 38 ANone 39)::(EA 39 AWeaken 76)::(EA 40 AWeaken 41)::(EA 41 (AAssign
  V_inflate_block__tmp None) 42)::(EA 42 ANone 43)::(EA 43 AWeaken 76)::
  (EA 44 AWeaken 45)::(EA 45 (AAssign V_inflate_block_incnt
  (Some (EAdd (EVar V_inflate_block_incnt) (ENum (-1))))) 46)::
  (EA 46 AWeaken 47)::(EA 47 (AGuard
  (fun s => ((eval (EAdd (EVar V_inflate_block_incnt) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 49)::(EA 47 (AGuard
  (fun s => ((eval (EAdd (EVar V_inflate_block_incnt) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 48)::(EA 48 AWeaken 51)::(EA 49 AWeaken 50)::
  (EA 50 ANone 57)::(EA 50 ANone 51)::(EA 51 (AAssign V_inflate_block_b
  None) 52)::(EA 52 (AAssign V_inflate_block_k
  (Some (EAdd (EVar V_inflate_block_k) (ENum (8))))) 53)::(EA 53 ANone 54)::
  (EA 54 ANone 55)::(EA 55 (AAssign V_inflate_block_z (Some (EAdd (ENum (1))
  (EVar V_inflate_block_z)))) 56)::(EA 56 AWeaken 15)::(EA 57 (AAssign
  V_inflate_block__tmp (Some (ENum (1)))) 58)::(EA 58 ANone 59)::
  (EA 59 AWeaken 76)::(EA 60 AWeaken 61)::(EA 61 (AAssign
  V_inflate_block_incnt (Some (EAdd (EVar V_inflate_block_incnt)
  (ENum (-1))))) 62)::(EA 62 AWeaken 63)::(EA 63 (AGuard
  (fun s => ((eval (EAdd (EVar V_inflate_block_incnt) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 65)::(EA 63 (AGuard
  (fun s => ((eval (EAdd (EVar V_inflate_block_incnt) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 64)::(EA 64 AWeaken 67)::(EA 65 AWeaken 66)::
  (EA 66 ANone 73)::(EA 66 ANone 67)::(EA 67 (AAssign V_inflate_block_b
  None) 68)::(EA 68 (AAssign V_inflate_block_k
  (Some (EAdd (EVar V_inflate_block_k) (ENum (8))))) 69)::(EA 69 ANone 70)::
  (EA 70 ANone 71)::(EA 71 (AAssign V_inflate_block_z (Some (EAdd (ENum (1))
  (EVar V_inflate_block_z)))) 72)::(EA 72 AWeaken 8)::(EA 73 (AAssign
  V_inflate_block__tmp (Some (ENum (1)))) 74)::(EA 74 ANone 75)::
  (EA 75 AWeaken 76)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_inflate_block => Pedges_inflate_block
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_inflate_block => 76
     end)%positive;
  var_global := var_global
}.

Definition ai_inflate_block (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 3 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 4 => (-1 * s V_inflate_block_k <= 0 /\ 1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 5 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 6 => (1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 7 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_z <= 0)%Z
   | 8 => (-1 * s V_inflate_block_z <= 0)%Z
   | 9 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k + 1 <= 0)%Z
   | 10 => (-1 * s V_inflate_block_k + 1 <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 11 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k + 1 <= 0)%Z
   | 12 => (-1 * s V_inflate_block_k + 1 <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 13 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 14 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 15 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 16 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k + 2 <= 0)%Z
   | 17 => (-1 * s V_inflate_block_k + 2 <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 18 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k + 2 <= 0)%Z
   | 19 => (-1 * s V_inflate_block_k + 2 <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 20 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 21 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 22 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0)%Z
   | 23 => (-1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 24 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0)%Z
   | 25 => (-1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 26 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0)%Z
   | 27 => (-1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 28 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0)%Z
   | 29 => (-1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 30 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block__tmp + -2 <= 0 /\ -1 * s V_inflate_block__tmp + 2 <= 0)%Z
   | 31 => (-1 * s V_inflate_block__tmp + 2 <= 0 /\ 1 * s V_inflate_block__tmp + -2 <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 32 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block_t + -1 <= 0 /\ -1 * s V_inflate_block_t + 1 <= 0)%Z
   | 33 => (-1 * s V_inflate_block_t + 1 <= 0 /\ 1 * s V_inflate_block_t + -1 <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 34 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block_t + -1 <= 0 /\ -1 * s V_inflate_block_t + 1 <= 0)%Z
   | 35 => (-1 * s V_inflate_block_t + 1 <= 0 /\ 1 * s V_inflate_block_t + -1 <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 36 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block_t <= 0 /\ -1 * s V_inflate_block_t <= 0)%Z
   | 37 => (-1 * s V_inflate_block_t <= 0 /\ 1 * s V_inflate_block_t <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 38 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block_t <= 0 /\ -1 * s V_inflate_block_t <= 0)%Z
   | 39 => (-1 * s V_inflate_block_t <= 0 /\ 1 * s V_inflate_block_t <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 40 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block_t + -2 <= 0 /\ -1 * s V_inflate_block_t + 2 <= 0)%Z
   | 41 => (-1 * s V_inflate_block_t + 2 <= 0 /\ 1 * s V_inflate_block_t + -2 <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 42 => (-1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ 1 * s V_inflate_block_t + -2 <= 0 /\ -1 * s V_inflate_block_t + 2 <= 0)%Z
   | 43 => (-1 * s V_inflate_block_t + 2 <= 0 /\ 1 * s V_inflate_block_t + -2 <= 0 /\ -1 * s V_inflate_block_bk <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 44 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0)%Z
   | 45 => (1 * s V_inflate_block_k + -1 <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 46 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0)%Z
   | 47 => (1 * s V_inflate_block_k + -1 <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 48 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0 /\ -1 * s V_inflate_block_incnt + 1 <= 0)%Z
   | 49 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0 /\ 1 * s V_inflate_block_incnt <= 0)%Z
   | 50 => (1 * s V_inflate_block_incnt <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 51 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0)%Z
   | 52 => (1 * s V_inflate_block_k + -1 <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0)%Z
   | 53 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -9 <= 0 /\ -1 * s V_inflate_block_k + 8 <= 0)%Z
   | 54 => (-1 * s V_inflate_block_k + 8 <= 0 /\ 1 * s V_inflate_block_k + -9 <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 55 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -9 <= 0 /\ -1 * s V_inflate_block_k + 8 <= 0)%Z
   | 56 => (-1 * s V_inflate_block_k + 8 <= 0 /\ 1 * s V_inflate_block_k + -9 <= 0 /\ -1 * s V_inflate_block_z + 1 <= 0)%Z
   | 57 => (-1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0 /\ 1 * s V_inflate_block_incnt <= 0)%Z
   | 58 => (1 * s V_inflate_block_incnt <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ 1 * s V_inflate_block__tmp + -1 <= 0 /\ -1 * s V_inflate_block__tmp + 1 <= 0)%Z
   | 59 => (-1 * s V_inflate_block__tmp + 1 <= 0 /\ 1 * s V_inflate_block__tmp + -1 <= 0 /\ -1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -1 <= 0 /\ 1 * s V_inflate_block_incnt <= 0)%Z
   | 60 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0)%Z
   | 61 => (1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 62 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0)%Z
   | 63 => (1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 64 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_incnt + 1 <= 0)%Z
   | 65 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0 /\ 1 * s V_inflate_block_incnt <= 0)%Z
   | 66 => (1 * s V_inflate_block_incnt <= 0 /\ 1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 67 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0)%Z
   | 68 => (1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 69 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -8 <= 0)%Z
   | 70 => (1 * s V_inflate_block_k + -8 <= 0 /\ -1 * s V_inflate_block_z <= 0)%Z
   | 71 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k + -8 <= 0)%Z
   | 72 => (1 * s V_inflate_block_k + -8 <= 0 /\ -1 * s V_inflate_block_z + 1 <= 0)%Z
   | 73 => (-1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0 /\ 1 * s V_inflate_block_incnt <= 0)%Z
   | 74 => (1 * s V_inflate_block_incnt <= 0 /\ 1 * s V_inflate_block_k <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block__tmp + -1 <= 0 /\ -1 * s V_inflate_block__tmp + 1 <= 0)%Z
   | 75 => (-1 * s V_inflate_block__tmp + 1 <= 0 /\ 1 * s V_inflate_block__tmp + -1 <= 0 /\ -1 * s V_inflate_block_z <= 0 /\ 1 * s V_inflate_block_k <= 0 /\ 1 * s V_inflate_block_incnt <= 0)%Z
   | 76 => (-1 * s V_inflate_block_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_inflate_block (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(10 - s V_inflate_block_bk) <= z)%Q
   | 2 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_bk) <= z)%Q
   | 3 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_bk) <= z)%Q
   | 4 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_bk) <= z)%Q
   | 5 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_bk) <= z)%Q
   | 6 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 7 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 8 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 9 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 10 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 11 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 12 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 13 => (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 14 => (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 15 => (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 16 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (9 - s V_inflate_block_k) (1
                                                                    - s V_inflate_block_k));
      (*-0.125 0*) F_max0_ge_0 (1 - s V_inflate_block_k)]
     (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 17 => (s V_inflate_block_z <= z)%Q
   | 18 => (s V_inflate_block_z <= z)%Q
   | 19 => (s V_inflate_block_z <= z)%Q
   | 20 => (s V_inflate_block_z <= z)%Q
   | 21 => (s V_inflate_block_z <= z)%Q
   | 22 => (s V_inflate_block_z <= z)%Q
   | 23 => (s V_inflate_block_z <= z)%Q
   | 24 => (s V_inflate_block_z <= z)%Q
   | 25 => (s V_inflate_block_z <= z)%Q
   | 26 => (s V_inflate_block_z <= z)%Q
   | 27 => (s V_inflate_block_z <= z)%Q
   | 28 => (s V_inflate_block_z <= z)%Q
   | 29 => (s V_inflate_block_z <= z)%Q
   | 30 => (s V_inflate_block_z <= z)%Q
   | 31 => (s V_inflate_block_z <= z)%Q
   | 32 => (s V_inflate_block_z <= z)%Q
   | 33 => (s V_inflate_block_z <= z)%Q
   | 34 => (s V_inflate_block_z <= z)%Q
   | 35 => (s V_inflate_block_z <= z)%Q
   | 36 => (s V_inflate_block_z <= z)%Q
   | 37 => (s V_inflate_block_z <= z)%Q
   | 38 => (s V_inflate_block_z <= z)%Q
   | 39 => (s V_inflate_block_z <= z)%Q
   | 40 => (s V_inflate_block_z <= z)%Q
   | 41 => (s V_inflate_block_z <= z)%Q
   | 42 => (s V_inflate_block_z <= z)%Q
   | 43 => (s V_inflate_block_z <= z)%Q
   | 44 => (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 45 => (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 46 => hints
     [(*0 0.125*) F_binom_monotonic 1 (F_max0_ge_arg (9 - s V_inflate_block_k)) (F_check_ge (9
                                                                    - s V_inflate_block_k) (0))]
     (s V_inflate_block_z + (1 # 8) * max0(9 - s V_inflate_block_k) <= z)%Q
   | 47 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 48 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 49 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 50 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 51 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 52 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 53 => ((17 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 54 => ((17 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 55 => ((17 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 56 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_inflate_block_z)) (F_check_ge (s V_inflate_block_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_inflate_block_z) (0))) (F_max0_ge_0 (s V_inflate_block_z));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                   - 
                                                                   s V_inflate_block_k) (0))) (F_max0_ge_0 (9
                                                                    - s V_inflate_block_k))]
     ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 57 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 58 => ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 59 => hints
     [(*-1 0*) F_one;
      (*-0.125 0*) F_max0_pre_decrement 1 (9 - s V_inflate_block_k) (8);
      (*-0.125 0*) F_max0_ge_0 (1 - s V_inflate_block_k);
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                   - 
                                                                   s V_inflate_block_k) (0))) (F_max0_ge_0 (9
                                                                    - s V_inflate_block_k))]
     ((9 # 8) - (1 # 8) * s V_inflate_block_k + s V_inflate_block_z <= z)%Q
   | 60 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 61 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 62 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 63 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 64 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (10 - s V_inflate_block_k) (8)]
     (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 65 => hints
     [(*0 0.125*) F_max0_pre_decrement 1 (10 - s V_inflate_block_k) (8)]
     (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 66 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(2 - s V_inflate_block_k) <= z)%Q
   | 67 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(2 - s V_inflate_block_k) <= z)%Q
   | 68 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(2 - s V_inflate_block_k) <= z)%Q
   | 69 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 70 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 71 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 72 => (s V_inflate_block_z + (1 # 8) * max0(10 - s V_inflate_block_k) <= z)%Q
   | 73 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(2 - s V_inflate_block_k) <= z)%Q
   | 74 => ((1 # 1) + s V_inflate_block_z
            + (1 # 8) * max0(2 - s V_inflate_block_k) <= z)%Q
   | 75 => hints
     [(*-1 0*) F_one; (*-0.125 0*) F_max0_ge_0 (2 - s V_inflate_block_k)]
     ((1 # 1) + s V_inflate_block_z + (1 # 8) * max0(2 - s V_inflate_block_k) <= z)%Q
   | 76 => (s V_inflate_block_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_inflate_block =>
    [mkPA Q (fun n z s => ai_inflate_block n s /\ annot0_inflate_block n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_inflate_block (proc_start P_inflate_block) s1 (proc_end P_inflate_block) s2 ->
    (s2 V_inflate_block_z <= (1 # 8) * max0(10 - s1 V_inflate_block_bk))%Q.
Proof.
  prove_bound ipa admissible_ipa P_inflate_block.
Qed.
