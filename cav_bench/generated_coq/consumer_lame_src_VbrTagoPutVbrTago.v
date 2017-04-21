Require Import pasta.Pasta.

Inductive proc: Type :=
  P_PutVbrTag.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_PutVbrTag_z := 1%positive.
Notation V_PutVbrTag__tmp := 2%positive.
Notation V_PutVbrTag__tmp1 := 3%positive.
Notation V_PutVbrTag__tmp2 := 4%positive.
Notation V_PutVbrTag_abyte := 5%positive.
Notation V_PutVbrTag_frameNum := 6%positive.
Notation V_PutVbrTag_i := 7%positive.
Notation V_PutVbrTag_lFileSize := 8%positive.
Notation V_PutVbrTag_nStreamIndex := 9%positive.
Notation V_PutVbrTag_nVbrNumFrames := 10%positive.
Notation V_PutVbrTag_nZeroStreamSize := 11%positive.
Notation V_PutVbrTag_lpszFileName := 12%positive.
Notation V_PutVbrTag_nVbrScale := 13%positive.
Notation V_PutVbrTag_nVersion := 14%positive.
Definition Pedges_PutVbrTag: list (edge proc) :=
  (EA 1 (AAssign V_PutVbrTag_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_PutVbrTag__tmp2 (Some (EVar V_PutVbrTag_nVbrScale))) 3)::(EA 3 (AAssign
  V_PutVbrTag__tmp1 (Some (EVar V_PutVbrTag_nVersion))) 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_PutVbrTag_nVbrNumFrames) s) = (eval (ENum (0))
  s))%Z)) 63)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_PutVbrTag_nVbrNumFrames) s) <> (eval (ENum (0))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 64)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 ANone 60)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_PutVbrTag_lFileSize None) 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_PutVbrTag_lFileSize) s) = (eval (ENum (0))
  s))%Z)) 56)::(EA 12 (AGuard (fun s => ((eval (EVar V_PutVbrTag_lFileSize)
  s) <> (eval (ENum (0)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_PutVbrTag__tmp1) s) = (eval (ENum (0))
  s))%Z)) 18)::(EA 14 (AGuard (fun s => ((eval (EVar V_PutVbrTag__tmp1) s) <>
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_PutVbrTag_abyte None) 17)::(EA 17 ANone 21)::(EA 18 AWeaken 19)::
  (EA 19 (AAssign V_PutVbrTag_abyte None) 20)::(EA 20 ANone 21)::
  (EA 21 (AAssign V_PutVbrTag_i (Some (ENum (1)))) 22)::(EA 22 ANone 23)::
  (EA 23 AWeaken 24)::(EA 24 (AGuard (fun s => ((eval (EVar V_PutVbrTag_i)
  s) < (eval (ENum (100)) s))%Z)) 45)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_PutVbrTag_i) s) >= (eval (ENum (100))
  s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EVar V_PutVbrTag_nZeroStreamSize))) 27)::(EA 27 (AAssign
  V_PutVbrTag_nStreamIndex (Some (EAdd (EVar V_PutVbrTag_nStreamIndex)
  (ENum (1))))) 28)::(EA 28 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (1))))) 29)::
  (EA 29 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (1))))) 30)::
  (EA 30 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (1))))) 31)::
  (EA 31 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (4))))) 32)::
  (EA 32 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (4))))) 33)::
  (EA 33 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (4))))) 34)::
  (EA 34 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (100))))) 35)::
  (EA 35 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (4))))) 36)::
  (EA 36 (AAssign V_PutVbrTag_nStreamIndex
  (Some (EAdd (EVar V_PutVbrTag_nStreamIndex) (ENum (20))))) 37)::
  (EA 37 AWeaken 38)::(EA 38 ANone 42)::(EA 38 ANone 39)::(EA 39 (AAssign
  V_PutVbrTag__tmp (Some (ENum (0)))) 40)::(EA 40 ANone 41)::
  (EA 41 AWeaken 67)::(EA 42 (AAssign V_PutVbrTag__tmp
  (Some (ENum (-1)))) 43)::(EA 43 ANone 44)::(EA 44 AWeaken 67)::
  (EA 45 AWeaken 46)::(EA 46 (AAssign V_PutVbrTag_frameNum None) 47)::
  (EA 47 AWeaken 48)::(EA 48 ANone 49)::(EA 48 ANone 50)::(EA 49 ANone 50)::
  (EA 50 ANone 51)::(EA 51 (AAssign V_PutVbrTag_i
  (Some (EAdd (EVar V_PutVbrTag_i) (ENum (1))))) 52)::(EA 52 ANone 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_PutVbrTag_z (Some (EAdd (ENum (1))
  (EVar V_PutVbrTag_z)))) 55)::(EA 55 AWeaken 24)::(EA 56 AWeaken 57)::
  (EA 57 (AAssign V_PutVbrTag__tmp (Some (ENum (-1)))) 58)::
  (EA 58 ANone 59)::(EA 59 AWeaken 67)::(EA 60 (AAssign V_PutVbrTag__tmp
  (Some (ENum (-1)))) 61)::(EA 61 ANone 62)::(EA 62 AWeaken 67)::
  (EA 63 AWeaken 64)::(EA 64 (AAssign V_PutVbrTag__tmp
  (Some (ENum (-1)))) 65)::(EA 65 ANone 66)::(EA 66 AWeaken 67)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_PutVbrTag => Pedges_PutVbrTag
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_PutVbrTag => 67
     end)%positive;
  var_global := var_global
}.

Definition ai_PutVbrTag (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 3 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 4 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 5 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 6 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 7 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 8 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 9 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 10 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 11 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 12 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 13 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 14 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 15 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 16 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 17 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 18 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag__tmp1 <= 0 /\ -1 * s V_PutVbrTag__tmp1 <= 0)%Z
   | 19 => (-1 * s V_PutVbrTag__tmp1 <= 0 /\ 1 * s V_PutVbrTag__tmp1 <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 20 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag__tmp1 <= 0 /\ -1 * s V_PutVbrTag__tmp1 <= 0)%Z
   | 21 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 22 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -1 <= 0 /\ -1 * s V_PutVbrTag_i + 1 <= 0)%Z
   | 23 => (-1 * s V_PutVbrTag_i + 1 <= 0 /\ 1 * s V_PutVbrTag_i + -1 <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 24 => (-1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 1 <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 25 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 26 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 27 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 28 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 29 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 30 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 31 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 32 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 33 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 34 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 35 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 36 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 37 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 38 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 39 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 40 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0 /\ 1 * s V_PutVbrTag__tmp <= 0 /\ -1 * s V_PutVbrTag__tmp <= 0)%Z
   | 41 => (-1 * s V_PutVbrTag__tmp <= 0 /\ 1 * s V_PutVbrTag__tmp <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 42 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 43 => (-1 * s V_PutVbrTag_i + 100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ -1 * s V_PutVbrTag__tmp + -1 <= 0)%Z
   | 44 => (-1 * s V_PutVbrTag__tmp + -1 <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 100 <= 0)%Z
   | 45 => (-1 * s V_PutVbrTag_i + 1 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -99 <= 0)%Z
   | 46 => (1 * s V_PutVbrTag_i + -99 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 1 <= 0)%Z
   | 47 => (-1 * s V_PutVbrTag_i + 1 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -99 <= 0)%Z
   | 48 => (1 * s V_PutVbrTag_i + -99 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 1 <= 0)%Z
   | 49 => (-1 * s V_PutVbrTag_i + 1 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -99 <= 0)%Z
   | 50 => (1 * s V_PutVbrTag_i + -99 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 1 <= 0)%Z
   | 51 => (-1 * s V_PutVbrTag_i + 1 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_i + -99 <= 0)%Z
   | 52 => (-1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 2 <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 53 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_i + 2 <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 54 => (-1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_i + 2 <= 0 /\ 1 * s V_PutVbrTag_i + -100 <= 0)%Z
   | 55 => (1 * s V_PutVbrTag_i + -100 <= 0 /\ -1 * s V_PutVbrTag_i + 2 <= 0 /\ -1 * s V_PutVbrTag_z + 1 <= 0)%Z
   | 56 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_lFileSize <= 0 /\ -1 * s V_PutVbrTag_lFileSize <= 0)%Z
   | 57 => (-1 * s V_PutVbrTag_lFileSize <= 0 /\ 1 * s V_PutVbrTag_lFileSize <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 58 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_lFileSize <= 0 /\ -1 * s V_PutVbrTag_lFileSize <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ -1 * s V_PutVbrTag__tmp + -1 <= 0)%Z
   | 59 => (-1 * s V_PutVbrTag__tmp + -1 <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ -1 * s V_PutVbrTag_lFileSize <= 0 /\ 1 * s V_PutVbrTag_lFileSize <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 60 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 61 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ -1 * s V_PutVbrTag__tmp + -1 <= 0)%Z
   | 62 => (-1 * s V_PutVbrTag__tmp + -1 <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ 1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0)%Z
   | 63 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_nVbrNumFrames <= 0 /\ -1 * s V_PutVbrTag_nVbrNumFrames <= 0)%Z
   | 64 => (-1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 65 => (1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ -1 * s V_PutVbrTag__tmp + -1 <= 0)%Z
   | 66 => (-1 * s V_PutVbrTag__tmp + -1 <= 0 /\ 1 * s V_PutVbrTag__tmp + 1 <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ 1 * s V_PutVbrTag_z <= 0)%Z
   | 67 => (1 * s V_PutVbrTag__tmp <= 0 /\ -1 * s V_PutVbrTag_z <= 0 /\ -1 * s V_PutVbrTag__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_PutVbrTag (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((99 # 1) <= z)%Q
   | 2 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 3 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 4 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 5 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 6 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 7 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 8 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 9 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 10 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 11 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 12 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 13 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 14 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 15 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 16 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 17 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 18 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 19 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 20 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 21 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 22 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 23 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 24 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 25 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 26 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 27 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 28 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 29 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 30 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 31 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 32 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 33 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 34 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 35 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 36 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 37 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (100
                                                              - s V_PutVbrTag_i) (0))) (F_max0_ge_0 (100
                                                                    - s V_PutVbrTag_i))]
     ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 38 => (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 39 => (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 40 => (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (100 - s V_PutVbrTag_i) (99
                                                                    - 
                                                                    s V_PutVbrTag_i));
      (*-1 0*) F_max0_ge_0 (99 - s V_PutVbrTag_i)]
     (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 42 => (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 43 => (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (100 - s V_PutVbrTag_i) (99
                                                                    - 
                                                                    s V_PutVbrTag_i));
      (*-1 0*) F_max0_ge_0 (99 - s V_PutVbrTag_i)]
     (s V_PutVbrTag_z + max0(100 - s V_PutVbrTag_i) <= z)%Q
   | 45 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 46 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 47 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 48 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 49 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 50 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 51 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 52 => ((101 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 53 => ((101 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 54 => ((101 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 55 => ((100 # 1) - s V_PutVbrTag_i + s V_PutVbrTag_z <= z)%Q
   | 56 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 57 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 58 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 59 => hints
     [(*-99 0*) F_one]
     ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 60 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 61 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 62 => hints
     [(*-99 0*) F_one]
     ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 63 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 64 => ((99 # 1) + s V_PutVbrTag_z <= z)%Q
   | 65 => (s V_PutVbrTag_z + (99 # 1) * max0(-s V_PutVbrTag__tmp) <= z)%Q
   | 66 => hints
     [(*-99 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_PutVbrTag__tmp)) (F_check_ge (0) (0))]
     (s V_PutVbrTag_z + (99 # 1) * max0(-s V_PutVbrTag__tmp) <= z)%Q
   | 67 => (s V_PutVbrTag_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_PutVbrTag =>
    [mkPA Q (fun n z s => ai_PutVbrTag n s /\ annot0_PutVbrTag n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_PutVbrTag (proc_start P_PutVbrTag) s1 (proc_end P_PutVbrTag) s2 ->
    (s2 V_PutVbrTag_z <= (99 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_PutVbrTag.
Qed.
