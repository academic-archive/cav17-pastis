Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zsetcolorscreen.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zsetcolorscreen_z := 1%positive.
Notation V_zsetcolorscreen__tmp := 2%positive.
Notation V_zsetcolorscreen_code := 3%positive.
Notation V_zsetcolorscreen_code1 := 4%positive.
Notation V_zsetcolorscreen_es_code_ := 5%positive.
Notation V_zsetcolorscreen_i := 6%positive.
Notation V_zsetcolorscreen_space := 7%positive.
Notation V_zsetcolorscreen_op := 8%positive.
Definition Pedges_zsetcolorscreen: list (edge proc) :=
  (EA 1 (AAssign V_zsetcolorscreen_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_zsetcolorscreen_code (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_zsetcolorscreen_space (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_zsetcolorscreen_i (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_i)
  s) < (eval (ENum (4)) s))%Z)) 73)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_i) s) >= (eval (ENum (4))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 9 ANone 15)::
  (EA 10 (AAssign V_zsetcolorscreen_es_code_ None) 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_es_code_) s) <
  (eval (ENum (0)) s))%Z)) 69)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_es_code_) s) >= (eval (ENum (0))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::
  (EA 16 AWeaken 17)::(EA 17 ANone 19)::(EA 17 ANone 18)::(EA 18 ANone 20)::
  (EA 19 ANone 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::
  (EA 23 ANone 25)::(EA 23 ANone 24)::(EA 24 ANone 26)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 28 ANone 34)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 ANone 34)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_zsetcolorscreen_code None) 32)::(EA 32 ANone 33)::(EA 33 AWeaken 37)::
  (EA 34 (AAssign V_zsetcolorscreen_code (Some (ENum (-25)))) 35)::
  (EA 35 ANone 36)::(EA 36 AWeaken 37)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_code) s) >= (eval (ENum (0))
  s))%Z)) 39)::(EA 37 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_code)
  s) < (eval (ENum (0)) s))%Z)) 38)::(EA 38 AWeaken 60)::(EA 39 AWeaken 40)::
  (EA 40 (AAssign V_zsetcolorscreen_i (Some (ENum (0)))) 41)::
  (EA 41 ANone 42)::(EA 42 AWeaken 43)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_i) s) < (eval (ENum (4))
  s))%Z)) 45)::(EA 43 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_i)
  s) >= (eval (ENum (4)) s))%Z)) 44)::(EA 44 AWeaken 58)::
  (EA 45 AWeaken 46)::(EA 46 (AAssign V_zsetcolorscreen_code None) 47)::
  (EA 47 AWeaken 48)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_code) s) < (eval (ENum (0))
  s))%Z)) 56)::(EA 48 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_code)
  s) >= (eval (ENum (0)) s))%Z)) 49)::(EA 49 AWeaken 50)::(EA 50 ANone 51)::
  (EA 51 (AAssign V_zsetcolorscreen_i (Some (EAdd (EVar V_zsetcolorscreen_i)
  (ENum (1))))) 52)::(EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_zsetcolorscreen_z (Some (EAdd (ENum (1))
  (EVar V_zsetcolorscreen_z)))) 55)::(EA 55 AWeaken 43)::(EA 56 AWeaken 57)::
  (EA 57 ANone 58)::(EA 58 ANone 59)::(EA 59 AWeaken 60)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_code) s) < (eval (ENum (0))
  s))%Z)) 65)::(EA 60 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_code)
  s) >= (eval (ENum (0)) s))%Z)) 61)::(EA 61 AWeaken 62)::(EA 62 (AAssign
  V_zsetcolorscreen__tmp (Some (ENum (5)))) 63)::(EA 63 ANone 64)::
  (EA 64 AWeaken 92)::(EA 65 AWeaken 66)::(EA 66 (AAssign
  V_zsetcolorscreen__tmp (Some (EVar V_zsetcolorscreen_code))) 67)::
  (EA 67 ANone 68)::(EA 68 AWeaken 92)::(EA 69 AWeaken 70)::(EA 70 (AAssign
  V_zsetcolorscreen__tmp (Some (EVar V_zsetcolorscreen_es_code_))) 71)::
  (EA 71 ANone 72)::(EA 72 AWeaken 92)::(EA 73 AWeaken 74)::(EA 74 (AAssign
  V_zsetcolorscreen_code1 None) 75)::(EA 75 AWeaken 76)::(EA 76 (AGuard
  (fun s => ((eval (EVar V_zsetcolorscreen_code1) s) < (eval (ENum (0))
  s))%Z)) 88)::(EA 76 (AGuard (fun s => ((eval (EVar V_zsetcolorscreen_code1)
  s) >= (eval (ENum (0)) s))%Z)) 77)::(EA 77 AWeaken 78)::(EA 78 ANone 80)::
  (EA 78 ANone 79)::(EA 79 ANone 81)::(EA 80 ANone 81)::(EA 81 (AAssign
  V_zsetcolorscreen_space None) 82)::(EA 82 ANone 83)::(EA 83 (AAssign
  V_zsetcolorscreen_i (Some (EAdd (EVar V_zsetcolorscreen_i)
  (ENum (1))))) 84)::(EA 84 ANone 85)::(EA 85 ANone 86)::(EA 86 (AAssign
  V_zsetcolorscreen_z (Some (EAdd (ENum (1))
  (EVar V_zsetcolorscreen_z)))) 87)::(EA 87 AWeaken 7)::(EA 88 AWeaken 89)::
  (EA 89 (AAssign V_zsetcolorscreen__tmp
  (Some (EVar V_zsetcolorscreen_code1))) 90)::(EA 90 ANone 91)::
  (EA 91 AWeaken 92)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zsetcolorscreen => Pedges_zsetcolorscreen
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zsetcolorscreen => 92
     end)%positive;
  var_global := var_global
}.

Definition ai_zsetcolorscreen (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 3 => (-1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 4 => (-1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_space <= 0 /\ -1 * s V_zsetcolorscreen_space <= 0)%Z
   | 5 => (-1 * s V_zsetcolorscreen_space <= 0 /\ 1 * s V_zsetcolorscreen_space <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0)%Z
   | 6 => (-1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_space <= 0 /\ -1 * s V_zsetcolorscreen_space <= 0)%Z
   | 7 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 8 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 9 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 10 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 11 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 12 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 13 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_es_code_ <= 0)%Z
   | 14 => (-1 * s V_zsetcolorscreen_es_code_ <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 15 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 16 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 17 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 18 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 19 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 20 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 21 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 22 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 23 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 24 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 25 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 26 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 27 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 28 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 29 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 30 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 31 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 32 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 33 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 34 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 35 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0 /\ 1 * s V_zsetcolorscreen_code + 25 <= 0 /\ -1 * s V_zsetcolorscreen_code + -25 <= 0)%Z
   | 36 => (-1 * s V_zsetcolorscreen_code + -25 <= 0 /\ 1 * s V_zsetcolorscreen_code + 25 <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 37 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 38 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code + 1 <= 0)%Z
   | 39 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 40 => (-1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 41 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0)%Z
   | 42 => (-1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 43 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 44 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 45 => (-1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0)%Z
   | 46 => (1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 47 => (-1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0)%Z
   | 48 => (1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0)%Z
   | 49 => (-1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 50 => (-1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0)%Z
   | 51 => (-1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 52 => (-1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 53 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i + 1 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 54 => (-1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 55 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i + 1 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z + 1 <= 0)%Z
   | 56 => (-1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ 1 * s V_zsetcolorscreen_code + 1 <= 0)%Z
   | 57 => (1 * s V_zsetcolorscreen_code + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0)%Z
   | 58 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 59 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0)%Z
   | 60 => (1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 61 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0)%Z
   | 62 => (-1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 63 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen__tmp + -5 <= 0 /\ -1 * s V_zsetcolorscreen__tmp + 5 <= 0)%Z
   | 64 => (-1 * s V_zsetcolorscreen__tmp + 5 <= 0 /\ 1 * s V_zsetcolorscreen__tmp + -5 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 65 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code + 1 <= 0)%Z
   | 66 => (1 * s V_zsetcolorscreen_code + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 67 => (-1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code + 1 <= 0 /\ 1 * s V_zsetcolorscreen__tmp + 1 <= 0)%Z
   | 68 => (1 * s V_zsetcolorscreen__tmp + 1 <= 0 /\ 1 * s V_zsetcolorscreen_code + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | 69 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_es_code_ + 1 <= 0)%Z
   | 70 => (1 * s V_zsetcolorscreen_es_code_ + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 71 => (-1 * s V_zsetcolorscreen_i + 4 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_es_code_ + 1 <= 0 /\ 1 * s V_zsetcolorscreen__tmp + 1 <= 0)%Z
   | 72 => (1 * s V_zsetcolorscreen__tmp + 1 <= 0 /\ 1 * s V_zsetcolorscreen_es_code_ + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i + 4 <= 0)%Z
   | 73 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0)%Z
   | 74 => (1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 75 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0)%Z
   | 76 => (1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 77 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0)%Z
   | 78 => (-1 * s V_zsetcolorscreen_code1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 79 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0)%Z
   | 80 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0)%Z
   | 81 => (-1 * s V_zsetcolorscreen_code1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 82 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0)%Z
   | 83 => (-1 * s V_zsetcolorscreen_code1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 84 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i + 1 <= 0)%Z
   | 85 => (-1 * s V_zsetcolorscreen_i + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 86 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i + 1 <= 0)%Z
   | 87 => (-1 * s V_zsetcolorscreen_i + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_code1 <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_z + 1 <= 0)%Z
   | 88 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ 1 * s V_zsetcolorscreen_code1 + 1 <= 0)%Z
   | 89 => (1 * s V_zsetcolorscreen_code1 + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 90 => (1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ 1 * s V_zsetcolorscreen_code1 + 1 <= 0 /\ 1 * s V_zsetcolorscreen__tmp + 1 <= 0)%Z
   | 91 => (1 * s V_zsetcolorscreen__tmp + 1 <= 0 /\ 1 * s V_zsetcolorscreen_code1 + 1 <= 0 /\ 1 * s V_zsetcolorscreen_i + -3 <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_code <= 0 /\ 1 * s V_zsetcolorscreen_code <= 0)%Z
   | 92 => (1 * s V_zsetcolorscreen__tmp + -5 <= 0 /\ 1 * s V_zsetcolorscreen_i + -4 <= 0 /\ -1 * s V_zsetcolorscreen_i <= 0 /\ -1 * s V_zsetcolorscreen_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zsetcolorscreen (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_zsetcolorscreen_z <= z)%Q
   | 3 => ((8 # 1) + s V_zsetcolorscreen_z <= z)%Q
   | 4 => ((8 # 1) + s V_zsetcolorscreen_z <= z)%Q
   | 5 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
           + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 6 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
           + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 7 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
           + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 8 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
           + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 9 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
           + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 10 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 11 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 12 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 13 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 14 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 15 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 16 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 17 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 18 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 19 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 20 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 21 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 22 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 23 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 24 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 25 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 26 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 27 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 28 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 29 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 30 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 31 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 32 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 33 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 34 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 35 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 36 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 37 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_ge_0 (4 - s V_zsetcolorscreen_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsetcolorscreen_i)) (F_check_ge (0) (0))]
     (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
      + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 39 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_zsetcolorscreen_i) (3
                                                                    - s V_zsetcolorscreen_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_zsetcolorscreen_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsetcolorscreen_i)) (F_check_ge (s V_zsetcolorscreen_i) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4 - s V_zsetcolorscreen_i)) (F_check_ge (4
                                                                    - s V_zsetcolorscreen_i) (0))]
     (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
      + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 40 => ((4 # 1) + s V_zsetcolorscreen_z <= z)%Q
   | 41 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 42 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 43 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 44 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 45 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 46 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 47 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 48 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 49 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 50 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 51 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 52 => ((5 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 53 => ((5 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 54 => ((5 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 55 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 56 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 57 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 58 => ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 59 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_zsetcolorscreen_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_zsetcolorscreen_i))]
     ((4 # 1) - s V_zsetcolorscreen_i + s V_zsetcolorscreen_z <= z)%Q
   | 60 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 61 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 62 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 63 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 64 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_zsetcolorscreen_i) (3
                                                                    - s V_zsetcolorscreen_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_zsetcolorscreen_i)]
     (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 65 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 66 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 67 => (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_zsetcolorscreen_i) (3
                                                                    - s V_zsetcolorscreen_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_zsetcolorscreen_i)]
     (s V_zsetcolorscreen_z + max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 69 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 70 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 71 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 72 => hints
     [(*-2 0*) F_max0_monotonic (F_check_ge (4 - s V_zsetcolorscreen_i) (3
                                                                    - s V_zsetcolorscreen_i));
      (*-2 0*) F_max0_ge_0 (3 - s V_zsetcolorscreen_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsetcolorscreen_i)) (F_check_ge (0) (0))]
     (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
      + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 73 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 74 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 75 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 76 => (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 77 => hints
     [(*-2 0*) F_max0_pre_decrement 1 (4 - s V_zsetcolorscreen_i) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsetcolorscreen_z) (0))) (F_max0_ge_0 (s V_zsetcolorscreen_z))]
     (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
      + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 78 => ((2 # 1) + (2 # 1) * max0(3 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 79 => ((2 # 1) + (2 # 1) * max0(3 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 80 => ((2 # 1) + (2 # 1) * max0(3 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 81 => ((2 # 1) + (2 # 1) * max0(3 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 82 => ((2 # 1) + (2 # 1) * max0(3 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 83 => ((2 # 1) + (2 # 1) * max0(3 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_i) + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 84 => ((2 # 1) + max0(-1 + s V_zsetcolorscreen_i)
            + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 85 => ((2 # 1) + max0(-1 + s V_zsetcolorscreen_i)
            + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 86 => ((2 # 1) + max0(-1 + s V_zsetcolorscreen_i)
            + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
            + max0(s V_zsetcolorscreen_z) <= z)%Q
   | 87 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsetcolorscreen_i) (0))) (F_max0_ge_0 (s V_zsetcolorscreen_i));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_zsetcolorscreen_z)) (F_check_ge (-1
                                                                    + s V_zsetcolorscreen_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_zsetcolorscreen_i)) (F_check_ge (-1
                                                                    + s V_zsetcolorscreen_i) (0))]
     ((2 # 1) + max0(-1 + s V_zsetcolorscreen_i)
      + max0(-1 + s V_zsetcolorscreen_z)
      + (2 # 1) * max0(4 - s V_zsetcolorscreen_i) <= z)%Q
   | 88 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (4 - s V_zsetcolorscreen_i)) (F_check_ge (0) (0))]
     (s V_zsetcolorscreen_z + (2 # 1) * max0(4 - s V_zsetcolorscreen_i)
      + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 89 => (s V_zsetcolorscreen_z + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 90 => (s V_zsetcolorscreen_z + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 91 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsetcolorscreen_i)) (F_check_ge (0) (0))]
     (s V_zsetcolorscreen_z + max0(s V_zsetcolorscreen_i) <= z)%Q
   | 92 => (s V_zsetcolorscreen_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zsetcolorscreen =>
    [mkPA Q (fun n z s => ai_zsetcolorscreen n s /\ annot0_zsetcolorscreen n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zsetcolorscreen (proc_start P_zsetcolorscreen) s1 (proc_end P_zsetcolorscreen) s2 ->
    (s2 V_zsetcolorscreen_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zsetcolorscreen.
Qed.
