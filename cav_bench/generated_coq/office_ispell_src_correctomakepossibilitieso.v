Require Import pasta.Pasta.

Inductive proc: Type :=
  P_makepossibilities.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_makepossibilities_z := 1%positive.
Notation V_makepossibilities_compoundflag := 2%positive.
Notation V_makepossibilities_easypossibilities := 3%positive.
Notation V_makepossibilities_i := 4%positive.
Notation V_makepossibilities_maxposslen := 5%positive.
Notation V_makepossibilities_pcount := 6%positive.
Notation V_makepossibilities_sortit := 7%positive.
Notation V_makepossibilities_tryhardflag := 8%positive.
Notation V_makepossibilities_word := 9%positive.
Definition Pedges_makepossibilities: list (edge proc) :=
  (EA 1 (AAssign V_makepossibilities_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_makepossibilities_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_i) s) < (eval (ENum (100))
  s))%Z)) 72)::(EA 5 (AGuard (fun s => ((eval (EVar V_makepossibilities_i)
  s) >= (eval (ENum (100)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_makepossibilities_pcount (Some (ENum (0)))) 8)::(EA 8 (AAssign
  V_makepossibilities_maxposslen (Some (ENum (0)))) 9)::(EA 9 (AAssign
  V_makepossibilities_easypossibilities (Some (ENum (0)))) 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) < (eval (ENum (100))
  s))%Z)) 13)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >= (eval (ENum (100))
  s))%Z)) 12)::(EA 12 AWeaken 16)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) < (eval (ENum (100))
  s))%Z)) 18)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >= (eval (ENum (100))
  s))%Z)) 17)::(EA 17 AWeaken 21)::(EA 18 AWeaken 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) < (eval (ENum (100))
  s))%Z)) 23)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >= (eval (ENum (100))
  s))%Z)) 22)::(EA 22 AWeaken 26)::(EA 23 AWeaken 24)::(EA 24 ANone 25)::
  (EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) < (eval (ENum (100))
  s))%Z)) 28)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >= (eval (ENum (100))
  s))%Z)) 27)::(EA 27 AWeaken 31)::(EA 28 AWeaken 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_compoundflag) s) <>
  (eval (ENum (1)) s))%Z)) 33)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_compoundflag) s) =
  (eval (ENum (1)) s))%Z)) 32)::(EA 32 AWeaken 38)::(EA 33 AWeaken 34)::
  (EA 34 (AGuard (fun s => ((eval (EVar V_makepossibilities_pcount) s) <
  (eval (ENum (100)) s))%Z)) 36)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >= (eval (ENum (100))
  s))%Z)) 35)::(EA 35 AWeaken 38)::(EA 36 AWeaken 37)::(EA 37 ANone 38)::
  (EA 38 (AAssign V_makepossibilities_easypossibilities
  (Some (EVar V_makepossibilities_pcount))) 39)::(EA 39 AWeaken 40)::
  (EA 40 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_easypossibilities) s) =
  (eval (ENum (0)) s))%Z)) 45)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_easypossibilities) s) <>
  (eval (ENum (0)) s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_tryhardflag) s) <>
  (eval (ENum (0)) s))%Z)) 44)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_tryhardflag) s) =
  (eval (ENum (0)) s))%Z)) 43)::(EA 43 AWeaken 48)::(EA 44 AWeaken 46)::
  (EA 45 AWeaken 46)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_sortit) s) <> (eval (ENum (0))
  s))%Z)) 53)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_sortit) s) = (eval (ENum (0))
  s))%Z)) 49)::(EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >
  (eval (EVar V_makepossibilities_easypossibilities) s))%Z)) 52)::
  (EA 50 (AGuard (fun s => ((eval (EVar V_makepossibilities_pcount) s) <=
  (eval (EVar V_makepossibilities_easypossibilities) s))%Z)) 51)::
  (EA 51 AWeaken 71)::(EA 52 AWeaken 54)::(EA 53 AWeaken 54)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) <> (eval (ENum (0))
  s))%Z)) 56)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) = (eval (ENum (0))
  s))%Z)) 55)::(EA 55 AWeaken 71)::(EA 56 AWeaken 57)::(EA 57 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_easypossibilities) s) >
  (eval (ENum (0)) s))%Z)) 59)::(EA 57 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_easypossibilities) s) <=
  (eval (ENum (0)) s))%Z)) 58)::(EA 58 AWeaken 65)::(EA 59 AWeaken 60)::
  (EA 60 (AGuard (fun s => ((eval (EVar V_makepossibilities_sortit) s) <>
  (eval (ENum (0)) s))%Z)) 62)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_sortit) s) = (eval (ENum (0))
  s))%Z)) 61)::(EA 61 AWeaken 65)::(EA 62 AWeaken 63)::(EA 63 ANone 64)::
  (EA 64 AWeaken 65)::(EA 65 (AGuard
  (fun s => ((eval (EVar V_makepossibilities_pcount) s) >
  (eval (EVar V_makepossibilities_easypossibilities) s))%Z)) 67)::
  (EA 65 (AGuard (fun s => ((eval (EVar V_makepossibilities_pcount) s) <=
  (eval (EVar V_makepossibilities_easypossibilities) s))%Z)) 66)::
  (EA 66 AWeaken 69)::(EA 67 AWeaken 68)::(EA 68 ANone 69)::
  (EA 69 ANone 70)::(EA 70 AWeaken 71)::(EA 72 AWeaken 73)::
  (EA 73 ANone 74)::(EA 74 (AAssign V_makepossibilities_i
  (Some (EAdd (EVar V_makepossibilities_i) (ENum (1))))) 75)::
  (EA 75 ANone 76)::(EA 76 ANone 77)::(EA 77 (AAssign V_makepossibilities_z
  (Some (EAdd (ENum (1)) (EVar V_makepossibilities_z)))) 78)::
  (EA 78 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_makepossibilities => Pedges_makepossibilities
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_makepossibilities => 71
     end)%positive;
  var_global := var_global
}.

Definition ai_makepossibilities (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_z <= 0)%Z
   | 3 => (-1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i <= 0 /\ -1 * s V_makepossibilities_i <= 0)%Z
   | 4 => (-1 * s V_makepossibilities_i <= 0 /\ 1 * s V_makepossibilities_i <= 0 /\ 1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_z <= 0)%Z
   | 5 => (-1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0)%Z
   | 6 => (1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0)%Z
   | 7 => (-1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0)%Z
   | 8 => (1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 9 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 10 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 11 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 12 => (False)%Z
   | 13 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 14 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 15 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 16 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 17 => (False)%Z
   | 18 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 19 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 20 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 21 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 22 => (False)%Z
   | 23 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 24 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 25 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 26 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 27 => (False)%Z
   | 28 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 29 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 30 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 31 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 32 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_compoundflag + -1 <= 0 /\ -1 * s V_makepossibilities_compoundflag + 1 <= 0)%Z
   | 33 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 34 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 35 => (False)%Z
   | 36 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 37 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0)%Z
   | 38 => (-1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 39 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 40 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 41 => (False)%Z
   | 42 => (False)%Z
   | 43 => (False)%Z
   | 44 => (False)%Z
   | 45 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 46 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 47 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 48 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 49 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_sortit <= 0 /\ -1 * s V_makepossibilities_sortit <= 0)%Z
   | 50 => (-1 * s V_makepossibilities_sortit <= 0 /\ 1 * s V_makepossibilities_sortit <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 51 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_sortit <= 0 /\ -1 * s V_makepossibilities_sortit <= 0)%Z
   | 52 => (False)%Z
   | 53 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 54 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 55 => (-1 * s V_makepossibilities_pcount <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_easypossibilities <= 0)%Z
   | 56 => (False)%Z
   | 57 => (False)%Z
   | 58 => (False)%Z
   | 59 => (False)%Z
   | 60 => (False)%Z
   | 61 => (False)%Z
   | 62 => (False)%Z
   | 63 => (False)%Z
   | 64 => (False)%Z
   | 65 => (False)%Z
   | 66 => (False)%Z
   | 67 => (False)%Z
   | 68 => (False)%Z
   | 69 => (False)%Z
   | 70 => (False)%Z
   | 71 => (-1 * s V_makepossibilities_easypossibilities <= 0 /\ 1 * s V_makepossibilities_easypossibilities <= 0 /\ -1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_maxposslen <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 100 <= 0 /\ 1 * s V_makepossibilities_pcount <= 0 /\ -1 * s V_makepossibilities_pcount <= 0)%Z
   | 72 => (-1 * s V_makepossibilities_i <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -99 <= 0)%Z
   | 73 => (1 * s V_makepossibilities_i + -99 <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i <= 0)%Z
   | 74 => (-1 * s V_makepossibilities_i <= 0 /\ -1 * s V_makepossibilities_z <= 0 /\ 1 * s V_makepossibilities_i + -99 <= 0)%Z
   | 75 => (-1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 1 <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0)%Z
   | 76 => (1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_i + 1 <= 0 /\ -1 * s V_makepossibilities_z <= 0)%Z
   | 77 => (-1 * s V_makepossibilities_z <= 0 /\ -1 * s V_makepossibilities_i + 1 <= 0 /\ 1 * s V_makepossibilities_i + -100 <= 0)%Z
   | 78 => (1 * s V_makepossibilities_i + -100 <= 0 /\ -1 * s V_makepossibilities_i + 1 <= 0 /\ -1 * s V_makepossibilities_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_makepossibilities (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((100 # 1) <= z)%Q
   | 2 => ((100 # 1) + s V_makepossibilities_z <= z)%Q
   | 3 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 4 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 5 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 6 => hints
     [(*0 1.0101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (100
                                                                   - 
                                                                   s V_makepossibilities_i) (0))) (F_max0_ge_0 (100
                                                                    - s V_makepossibilities_i));
      (*0 0.010101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + 
                                                                    s V_makepossibilities_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_makepossibilities_i))]
     ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 7 => (-(1 # 1) + s V_makepossibilities_z
           + (1 # 99) * max0(-1 + s V_makepossibilities_i)
           + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 8 => (-(1 # 1) + s V_makepossibilities_z
           + (1 # 99) * max0(-1 + s V_makepossibilities_i)
           + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 9 => (-(1 # 1) + s V_makepossibilities_z
           + (1 # 99) * max0(-1 + s V_makepossibilities_i)
           + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 10 => hints
     [(*-0.010101 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                          + s V_makepossibilities_i)) (F_check_ge (-1
                                                                    + s V_makepossibilities_i) (0))]
     (-(1 # 1) + s V_makepossibilities_z
      + (1 # 99) * max0(-1 + s V_makepossibilities_i)
      + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 11 => (-(100 # 99) + (1 # 99) * s V_makepossibilities_i
            + s V_makepossibilities_z
            + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 12 => hints
     [(*-1.0101 0*) F_max0_pre_decrement 1 (100 - s V_makepossibilities_i) (1);
      (*-1.0101 0*) F_max0_ge_0 (99 - s V_makepossibilities_i);
      (*-0.010101 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_makepossibilities_i)) (F_check_ge (0) (0));
      (*0 0.010101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_makepossibilities_i) (0))) (F_max0_ge_0 (s V_makepossibilities_i))]
     (-(100 # 99) + (1 # 99) * s V_makepossibilities_i
      + s V_makepossibilities_z
      + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 13 => hints
     [(*0 0.010101*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + 
                                                                    s V_makepossibilities_i) (0))) (F_max0_ge_0 (-100
                                                                    + s V_makepossibilities_i))]
     (-(100 # 99) + (1 # 99) * s V_makepossibilities_i
      + s V_makepossibilities_z
      + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 14 => (s V_makepossibilities_z
            + (1 # 99) * max0(-100 + s V_makepossibilities_i)
            + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 15 => hints
     [(*-1.0101 0*) F_max0_monotonic (F_check_ge (100
                                                  - s V_makepossibilities_i) (99
                                                                    - s V_makepossibilities_i));
      (*-1.0101 0*) F_max0_ge_0 (99 - s V_makepossibilities_i);
      (*-0.010101 0*) F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                        + s V_makepossibilities_i)) (F_check_ge (0) (0))]
     (s V_makepossibilities_z
      + (1 # 99) * max0(-100 + s V_makepossibilities_i)
      + (100 # 99) * max0(100 - s V_makepossibilities_i) <= z)%Q
   | 16 => (s V_makepossibilities_z <= z)%Q
   | 17 => (s V_makepossibilities_z <= z)%Q
   | 18 => (s V_makepossibilities_z <= z)%Q
   | 19 => (s V_makepossibilities_z <= z)%Q
   | 20 => (s V_makepossibilities_z <= z)%Q
   | 21 => (s V_makepossibilities_z <= z)%Q
   | 22 => (s V_makepossibilities_z <= z)%Q
   | 23 => (s V_makepossibilities_z <= z)%Q
   | 24 => (s V_makepossibilities_z <= z)%Q
   | 25 => (s V_makepossibilities_z <= z)%Q
   | 26 => (s V_makepossibilities_z <= z)%Q
   | 27 => (s V_makepossibilities_z <= z)%Q
   | 28 => (s V_makepossibilities_z <= z)%Q
   | 29 => (s V_makepossibilities_z <= z)%Q
   | 30 => (s V_makepossibilities_z <= z)%Q
   | 31 => (s V_makepossibilities_z <= z)%Q
   | 32 => (s V_makepossibilities_z <= z)%Q
   | 33 => (s V_makepossibilities_z <= z)%Q
   | 34 => (s V_makepossibilities_z <= z)%Q
   | 35 => (s V_makepossibilities_z <= z)%Q
   | 36 => (s V_makepossibilities_z <= z)%Q
   | 37 => (s V_makepossibilities_z <= z)%Q
   | 38 => (s V_makepossibilities_z <= z)%Q
   | 39 => (s V_makepossibilities_z <= z)%Q
   | 40 => (s V_makepossibilities_z <= z)%Q
   | 41 => (s V_makepossibilities_z <= z)%Q
   | 42 => (s V_makepossibilities_z <= z)%Q
   | 43 => (s V_makepossibilities_z <= z)%Q
   | 44 => (s V_makepossibilities_z <= z)%Q
   | 45 => (s V_makepossibilities_z <= z)%Q
   | 46 => (s V_makepossibilities_z <= z)%Q
   | 47 => (s V_makepossibilities_z <= z)%Q
   | 48 => (s V_makepossibilities_z <= z)%Q
   | 49 => (s V_makepossibilities_z <= z)%Q
   | 50 => (s V_makepossibilities_z <= z)%Q
   | 51 => (s V_makepossibilities_z <= z)%Q
   | 52 => (s V_makepossibilities_z <= z)%Q
   | 53 => (s V_makepossibilities_z <= z)%Q
   | 54 => (s V_makepossibilities_z <= z)%Q
   | 55 => (s V_makepossibilities_z <= z)%Q
   | 56 => (s V_makepossibilities_z <= z)%Q
   | 57 => (s V_makepossibilities_z <= z)%Q
   | 58 => (s V_makepossibilities_z <= z)%Q
   | 59 => (s V_makepossibilities_z <= z)%Q
   | 60 => (s V_makepossibilities_z <= z)%Q
   | 61 => (s V_makepossibilities_z <= z)%Q
   | 62 => (s V_makepossibilities_z <= z)%Q
   | 63 => (s V_makepossibilities_z <= z)%Q
   | 64 => (s V_makepossibilities_z <= z)%Q
   | 65 => (s V_makepossibilities_z <= z)%Q
   | 66 => (s V_makepossibilities_z <= z)%Q
   | 67 => (s V_makepossibilities_z <= z)%Q
   | 68 => (s V_makepossibilities_z <= z)%Q
   | 69 => (s V_makepossibilities_z <= z)%Q
   | 70 => (s V_makepossibilities_z <= z)%Q
   | 71 => (s V_makepossibilities_z <= z)%Q
   | 72 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 73 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 74 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 75 => ((101 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 76 => ((101 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 77 => ((101 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | 78 => ((100 # 1) - s V_makepossibilities_i + s V_makepossibilities_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_makepossibilities =>
    [mkPA Q (fun n z s => ai_makepossibilities n s /\ annot0_makepossibilities n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_makepossibilities (proc_start P_makepossibilities) s1 (proc_end P_makepossibilities) s2 ->
    (s2 V_makepossibilities_z <= (100 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_makepossibilities.
Qed.
