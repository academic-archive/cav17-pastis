Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BZ2_bzReadOpen.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BZ2_bzReadOpen_z := 1%positive.
Notation V_BZ2_bzReadOpen__tmp := 2%positive.
Notation V_BZ2_bzReadOpen__tmp1 := 3%positive.
Notation V_BZ2_bzReadOpen__tmp2 := 4%positive.
Notation V_BZ2_bzReadOpen_ret := 5%positive.
Notation V_BZ2_bzReadOpen_bzerror := 6%positive.
Notation V_BZ2_bzReadOpen_f := 7%positive.
Notation V_BZ2_bzReadOpen_nUnused := 8%positive.
Notation V_BZ2_bzReadOpen_small := 9%positive.
Notation V_BZ2_bzReadOpen_unused := 10%positive.
Notation V_BZ2_bzReadOpen_verbosity := 11%positive.
Definition Pedges_BZ2_bzReadOpen: list (edge proc) :=
  (EA 1 (AAssign V_BZ2_bzReadOpen_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_BZ2_bzReadOpen__tmp1 (Some (EVar V_BZ2_bzReadOpen_verbosity))) 3)::
  (EA 3 (AAssign V_BZ2_bzReadOpen__tmp
  (Some (EVar V_BZ2_bzReadOpen_small))) 4)::(EA 4 (AAssign
  V_BZ2_bzReadOpen__tmp2 (Some (EVar V_BZ2_bzReadOpen_nUnused))) 5)::
  (EA 5 AWeaken 6)::(EA 6 ANone 8)::(EA 6 ANone 7)::(EA 7 AWeaken 10)::
  (EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 ANone 12)::(EA 10 ANone 11)::
  (EA 11 AWeaken 14)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 ANone 96)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp) s) <> (eval (ENum (0))
  s))%Z)) 18)::(EA 16 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp)
  s) = (eval (ENum (0)) s))%Z)) 17)::(EA 17 AWeaken 21)::(EA 18 AWeaken 19)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp) s) <>
  (eval (ENum (1)) s))%Z)) 95)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp) s) = (eval (ENum (1))
  s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp1) s) < (eval (ENum (0))
  s))%Z)) 94)::(EA 21 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp1)
  s) >= (eval (ENum (0)) s))%Z)) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp1) s) > (eval (ENum (4))
  s))%Z)) 93)::(EA 23 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp1)
  s) <= (eval (ENum (4)) s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 27)::
  (EA 25 ANone 26)::(EA 26 AWeaken 30)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2) s) <> (eval (ENum (0))
  s))%Z)) 92)::(EA 28 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2)
  s) = (eval (ENum (0)) s))%Z)) 29)::(EA 29 AWeaken 30)::(EA 30 ANone 32)::
  (EA 30 ANone 31)::(EA 31 AWeaken 37)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2) s) < (eval (ENum (0))
  s))%Z)) 91)::(EA 33 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2)
  s) >= (eval (ENum (0)) s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2) s) > (eval (ENum (5000))
  s))%Z)) 90)::(EA 35 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2)
  s) <= (eval (ENum (5000)) s))%Z)) 36)::(EA 36 AWeaken 37)::
  (EA 37 ANone 81)::(EA 37 ANone 38)::(EA 38 AWeaken 39)::(EA 39 ANone 72)::
  (EA 39 ANone 40)::(EA 40 AWeaken 41)::(EA 41 ANone 43)::(EA 41 ANone 42)::
  (EA 42 AWeaken 45)::(EA 43 ANone 44)::(EA 44 AWeaken 45)::
  (EA 45 ANone 46)::(EA 45 ANone 47)::(EA 46 ANone 47)::(EA 47 ANone 48)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2) s) > (eval (ENum (0))
  s))%Z)) 66)::(EA 49 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen__tmp2)
  s) <= (eval (ENum (0)) s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 (AAssign
  V_BZ2_bzReadOpen_ret None) 52)::(EA 52 AWeaken 53)::(EA 53 (AGuard
  (fun s => ((eval (EVar V_BZ2_bzReadOpen_ret) s) <> (eval (ENum (0))
  s))%Z)) 57)::(EA 53 (AGuard (fun s => ((eval (EVar V_BZ2_bzReadOpen_ret)
  s) = (eval (ENum (0)) s))%Z)) 54)::(EA 54 AWeaken 55)::(EA 55 ANone 56)::
  (EA 56 AWeaken 105)::(EA 57 AWeaken 58)::(EA 58 ANone 60)::
  (EA 58 ANone 59)::(EA 59 AWeaken 62)::(EA 60 ANone 61)::
  (EA 61 AWeaken 62)::(EA 62 ANone 63)::(EA 62 ANone 64)::(EA 63 ANone 64)::
  (EA 64 ANone 65)::(EA 65 AWeaken 105)::(EA 66 AWeaken 67)::(EA 67 (AAssign
  V_BZ2_bzReadOpen__tmp2 (Some (EAdd (EVar V_BZ2_bzReadOpen__tmp2)
  (ENum (-1))))) 68)::(EA 68 ANone 69)::(EA 69 ANone 70)::(EA 70 (AAssign
  V_BZ2_bzReadOpen_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_bzReadOpen_z)))) 71)::(EA 71 AWeaken 49)::(EA 72 AWeaken 73)::
  (EA 73 ANone 75)::(EA 73 ANone 74)::(EA 74 AWeaken 77)::(EA 75 ANone 76)::
  (EA 76 AWeaken 77)::(EA 77 ANone 78)::(EA 77 ANone 79)::(EA 78 ANone 79)::
  (EA 79 ANone 80)::(EA 80 AWeaken 105)::(EA 81 AWeaken 82)::
  (EA 82 ANone 84)::(EA 82 ANone 83)::(EA 83 AWeaken 86)::(EA 84 ANone 85)::
  (EA 85 AWeaken 86)::(EA 86 ANone 87)::(EA 86 ANone 88)::(EA 87 ANone 88)::
  (EA 88 ANone 89)::(EA 89 AWeaken 105)::(EA 90 AWeaken 97)::
  (EA 91 AWeaken 97)::(EA 92 AWeaken 97)::(EA 93 AWeaken 97)::
  (EA 94 AWeaken 97)::(EA 95 AWeaken 97)::(EA 96 AWeaken 97)::
  (EA 97 ANone 99)::(EA 97 ANone 98)::(EA 98 AWeaken 101)::
  (EA 99 ANone 100)::(EA 100 AWeaken 101)::(EA 101 ANone 102)::
  (EA 101 ANone 103)::(EA 102 ANone 103)::(EA 103 ANone 104)::
  (EA 104 AWeaken 105)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BZ2_bzReadOpen => Pedges_BZ2_bzReadOpen
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BZ2_bzReadOpen => 105
     end)%positive;
  var_global := var_global
}.

Definition ai_BZ2_bzReadOpen (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 3 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 4 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 5 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 6 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 7 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 8 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 9 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 10 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 11 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 12 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 13 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 14 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 15 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 16 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 17 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0)%Z
   | 18 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 19 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 20 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 22 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0)%Z
   | 23 => (-1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 24 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 25 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 26 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 27 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 28 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 29 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 30 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 31 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 32 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 33 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 34 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 35 => (-1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 36 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 + -5000 <= 0)%Z
   | 37 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 38 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 39 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 40 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 41 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 42 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 43 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 44 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 45 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 46 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 47 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 48 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 49 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 50 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 51 => (1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 52 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 53 => (1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 54 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ 1 * s V_BZ2_bzReadOpen_ret <= 0 /\ -1 * s V_BZ2_bzReadOpen_ret <= 0)%Z
   | 55 => (-1 * s V_BZ2_bzReadOpen_ret <= 0 /\ 1 * s V_BZ2_bzReadOpen_ret <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 56 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ 1 * s V_BZ2_bzReadOpen_ret <= 0 /\ -1 * s V_BZ2_bzReadOpen_ret <= 0)%Z
   | 57 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 58 => (1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 59 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 60 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 61 => (1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 62 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 63 => (1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 64 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 65 => (1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 66 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 + 1 <= 0)%Z
   | 67 => (-1 * s V_BZ2_bzReadOpen__tmp2 + 1 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 68 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 69 => (-1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0)%Z
   | 70 => (1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 <= 0)%Z
   | 71 => (-1 * s V_BZ2_bzReadOpen__tmp2 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen_z + 1 <= 0)%Z
   | 72 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 73 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 74 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 75 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 76 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 77 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 78 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 79 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 80 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 81 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 82 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 83 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 84 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 85 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 86 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 87 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 88 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 89 => (1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 90 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp2 + 5001 <= 0)%Z
   | 91 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp2 + 1 <= 0)%Z
   | 92 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + -4 <= 0)%Z
   | 93 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp1 + 5 <= 0)%Z
   | 94 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp + -1 <= 0 /\ -1 * s V_BZ2_bzReadOpen__tmp <= 0 /\ 1 * s V_BZ2_bzReadOpen__tmp1 + 1 <= 0)%Z
   | 95 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 96 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 97 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 98 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 99 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 100 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 101 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 102 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 103 => (-1 * s V_BZ2_bzReadOpen_z <= 0 /\ 1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 104 => (1 * s V_BZ2_bzReadOpen_z <= 0 /\ -1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | 105 => (-1 * s V_BZ2_bzReadOpen_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BZ2_bzReadOpen (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_BZ2_bzReadOpen_nUnused) <= z)%Q
   | 2 => (max0(s V_BZ2_bzReadOpen_nUnused) <= z)%Q
   | 3 => (max0(s V_BZ2_bzReadOpen_nUnused) <= z)%Q
   | 4 => (max0(s V_BZ2_bzReadOpen_nUnused) <= z)%Q
   | 5 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 6 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 7 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 8 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 9 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 10 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 11 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 12 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 13 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 14 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 15 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 16 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 17 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 18 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 19 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 20 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 21 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 22 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 23 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 24 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 25 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 26 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 27 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 28 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 29 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 30 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_bzReadOpen__tmp) (0))) (F_max0_ge_0 (s V_BZ2_bzReadOpen__tmp))]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 32 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 33 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 34 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_bzReadOpen__tmp) (0))) (F_max0_ge_0 (s V_BZ2_bzReadOpen__tmp))]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 35 => (-s V_BZ2_bzReadOpen__tmp + max0(s V_BZ2_bzReadOpen__tmp)
            + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 36 => (-s V_BZ2_bzReadOpen__tmp + max0(s V_BZ2_bzReadOpen__tmp)
            + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 37 => (-s V_BZ2_bzReadOpen__tmp + max0(s V_BZ2_bzReadOpen__tmp)
            + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 38 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_bzReadOpen_z) (0))) (F_max0_ge_0 (s V_BZ2_bzReadOpen_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BZ2_bzReadOpen_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_BZ2_bzReadOpen_z) (0))) (F_max0_ge_0 (-
                                                                    s V_BZ2_bzReadOpen_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_bzReadOpen__tmp)) (F_check_ge (s V_BZ2_bzReadOpen__tmp) (0))]
     (-s V_BZ2_bzReadOpen__tmp + max0(s V_BZ2_bzReadOpen__tmp)
      + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 39 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 40 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 41 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 42 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 43 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 44 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 45 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 46 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 47 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 48 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 49 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 50 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 51 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 52 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_bzReadOpen_z)) (F_check_ge (s V_BZ2_bzReadOpen_z) (0))]
     (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 53 => (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 54 => (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 55 => (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 57 => (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 58 => (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 59 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_BZ2_bzReadOpen__tmp2)) (F_check_ge (0) (0))]
     (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 60 => (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 61 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_BZ2_bzReadOpen__tmp2)) (F_check_ge (0) (0))]
     (s V_BZ2_bzReadOpen_z + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 62 => (s V_BZ2_bzReadOpen_z <= z)%Q
   | 63 => (s V_BZ2_bzReadOpen_z <= z)%Q
   | 64 => (s V_BZ2_bzReadOpen_z <= z)%Q
   | 65 => (s V_BZ2_bzReadOpen_z <= z)%Q
   | 66 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_BZ2_bzReadOpen__tmp2) (1)]
     (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 67 => ((1 # 1) + max0(-1 + s V_BZ2_bzReadOpen__tmp2)
            + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 68 => ((1 # 1) + max0(s V_BZ2_bzReadOpen__tmp2)
            + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 69 => ((1 # 1) + max0(s V_BZ2_bzReadOpen__tmp2)
            + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 70 => ((1 # 1) + max0(s V_BZ2_bzReadOpen__tmp2)
            + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 71 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_bzReadOpen_z) (0))) (F_max0_ge_0 (s V_BZ2_bzReadOpen_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_BZ2_bzReadOpen_z)) (F_check_ge (-1
                                                                    + s V_BZ2_bzReadOpen_z) (0))]
     ((1 # 1) + max0(-1 + s V_BZ2_bzReadOpen_z)
      + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 72 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 73 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 74 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_BZ2_bzReadOpen__tmp2)) (F_check_ge (0) (0))]
     (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 75 => (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 76 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_BZ2_bzReadOpen__tmp2)) (F_check_ge (0) (0))]
     (max0(s V_BZ2_bzReadOpen__tmp2) + max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 77 => (max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 78 => (max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 79 => (max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 80 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_bzReadOpen_z)) (F_check_ge (s V_BZ2_bzReadOpen_z) (0))]
     (max0(s V_BZ2_bzReadOpen_z) <= z)%Q
   | 81 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_bzReadOpen__tmp)) (F_check_ge (s V_BZ2_bzReadOpen__tmp) (0))]
     (-s V_BZ2_bzReadOpen__tmp + max0(s V_BZ2_bzReadOpen__tmp)
      + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 82 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 83 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BZ2_bzReadOpen_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_BZ2_bzReadOpen__tmp2)) (F_check_ge (0) (0))]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 84 => (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 85 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BZ2_bzReadOpen_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_BZ2_bzReadOpen__tmp2)) (F_check_ge (0) (0))]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 86 => (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 87 => (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 88 => (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 89 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_BZ2_bzReadOpen_z) (0))) (F_max0_ge_0 (-
                                                                    s V_BZ2_bzReadOpen_z))]
     (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 90 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_BZ2_bzReadOpen__tmp2) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_bzReadOpen__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_BZ2_bzReadOpen__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_BZ2_bzReadOpen__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_BZ2_bzReadOpen__tmp))]
     (-s V_BZ2_bzReadOpen__tmp + max0(s V_BZ2_bzReadOpen__tmp)
      + max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 91 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 92 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 93 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 94 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 95 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 96 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_BZ2_bzReadOpen__tmp2) (-1
                                                                    + s V_BZ2_bzReadOpen__tmp2));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_bzReadOpen__tmp2)]
     (max0(s V_BZ2_bzReadOpen__tmp2) <= z)%Q
   | 97 => (0 <= z)%Q
   | 98 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BZ2_bzReadOpen_z)) (F_check_ge (0) (0))]
     (0 <= z)%Q
   | 99 => (0 <= z)%Q
   | 100 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BZ2_bzReadOpen_z)) (F_check_ge (0) (0))]
     (0 <= z)%Q
   | 101 => (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 102 => (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 103 => (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 104 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_BZ2_bzReadOpen_z) (0))) (F_max0_ge_0 (-
                                                                    s V_BZ2_bzReadOpen_z))]
     (-max0(-s V_BZ2_bzReadOpen_z) <= z)%Q
   | 105 => (s V_BZ2_bzReadOpen_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BZ2_bzReadOpen =>
    [mkPA Q (fun n z s => ai_BZ2_bzReadOpen n s /\ annot0_BZ2_bzReadOpen n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BZ2_bzReadOpen (proc_start P_BZ2_bzReadOpen) s1 (proc_end P_BZ2_bzReadOpen) s2 ->
    (s2 V_BZ2_bzReadOpen_z <= max0(s1 V_BZ2_bzReadOpen_nUnused))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BZ2_bzReadOpen.
Qed.
