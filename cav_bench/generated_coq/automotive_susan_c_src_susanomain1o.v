Require Import pasta.Pasta.

Inductive proc: Type :=
  P_main1.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_main1_z := 1%positive.
Notation V_main1__tmp := 2%positive.
Notation V_main1_argindex := 3%positive.
Notation V_main1_bt := 4%positive.
Notation V_main1_drawing_mode := 5%positive.
Notation V_main1_max_no_corners := 6%positive.
Notation V_main1_max_no_edges := 7%positive.
Notation V_main1_mode := 8%positive.
Notation V_main1_principle := 9%positive.
Notation V_main1_susan_quick := 10%positive.
Notation V_main1_thin_post_proc := 11%positive.
Notation V_main1_three_by_three := 12%positive.
Notation V_main1_argc := 13%positive.
Notation V_main1_argv := 14%positive.
Definition Pedges_main1: list (edge proc) :=
  (EA 1 (AAssign V_main1_z (Some (ENum (0)))) 2)::(EA 2 (AAssign V_main1__tmp
  (Some (EVar V_main1_argc))) 3)::(EA 3 (AAssign V_main1_argindex
  (Some (ENum (3)))) 4)::(EA 4 (AAssign V_main1_bt (Some (ENum (20)))) 5)::
  (EA 5 (AAssign V_main1_principle (Some (ENum (0)))) 6)::(EA 6 (AAssign
  V_main1_thin_post_proc (Some (ENum (1)))) 7)::(EA 7 (AAssign
  V_main1_three_by_three (Some (ENum (0)))) 8)::(EA 8 (AAssign
  V_main1_drawing_mode (Some (ENum (0)))) 9)::(EA 9 (AAssign
  V_main1_susan_quick (Some (ENum (0)))) 10)::(EA 10 (AAssign
  V_main1_max_no_corners (Some (ENum (1850)))) 11)::(EA 11 (AAssign
  V_main1_max_no_edges (Some (ENum (2650)))) 12)::(EA 12 (AAssign
  V_main1_mode (Some (ENum (0)))) 13)::(EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_main1__tmp) s) < (eval (ENum (3)) s))%Z)) 16)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_main1__tmp) s) >= (eval (ENum (3))
  s))%Z)) 15)::(EA 15 AWeaken 18)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_main1_argindex) s) < (eval (EVar V_main1__tmp)
  s))%Z)) 70)::(EA 20 (AGuard (fun s => ((eval (EVar V_main1_argindex) s) >=
  (eval (EVar V_main1__tmp) s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_main1_principle) s) = (eval (ENum (1))
  s))%Z)) 24)::(EA 22 (AGuard (fun s => ((eval (EVar V_main1_principle) s) <>
  (eval (ENum (1)) s))%Z)) 23)::(EA 23 AWeaken 31)::(EA 24 AWeaken 25)::
  (EA 25 (AGuard (fun s => ((eval (EVar V_main1_mode) s) = (eval (ENum (0))
  s))%Z)) 27)::(EA 25 (AGuard (fun s => ((eval (EVar V_main1_mode) s) <>
  (eval (ENum (0)) s))%Z)) 26)::(EA 26 AWeaken 31)::(EA 27 AWeaken 28)::
  (EA 28 (AAssign V_main1_mode (Some (ENum (1)))) 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 ANone 68)::(EA 31 ANone 67)::(EA 31 ANone 44)::
  (EA 31 ANone 32)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_main1_principle) s) <> (eval (ENum (0))
  s))%Z)) 41)::(EA 33 (AGuard (fun s => ((eval (EVar V_main1_principle) s) =
  (eval (ENum (0)) s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_main1_susan_quick) s) <> (eval (ENum (0))
  s))%Z)) 38)::(EA 35 (AGuard (fun s => ((eval (EVar V_main1_susan_quick)
  s) = (eval (ENum (0)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 ANone 40)::
  (EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 ANone 43)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 43 ANone 68)::
  (EA 44 AWeaken 45)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_main1_principle) s) <> (eval (ENum (0))
  s))%Z)) 59)::(EA 45 (AGuard (fun s => ((eval (EVar V_main1_principle) s) =
  (eval (ENum (0)) s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 (AGuard
  (fun s => ((eval (EVar V_main1_three_by_three) s) <> (eval (ENum (0))
  s))%Z)) 51)::(EA 47 (AGuard (fun s => ((eval (EVar V_main1_three_by_three)
  s) = (eval (ENum (0)) s))%Z)) 48)::(EA 48 AWeaken 49)::(EA 49 ANone 50)::
  (EA 50 AWeaken 54)::(EA 51 AWeaken 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_main1_thin_post_proc) s) <> (eval (ENum (0))
  s))%Z)) 56)::(EA 54 (AGuard (fun s => ((eval (EVar V_main1_thin_post_proc)
  s) = (eval (ENum (0)) s))%Z)) 55)::(EA 55 AWeaken 58)::(EA 56 AWeaken 57)::
  (EA 57 ANone 58)::(EA 58 ANone 66)::(EA 59 AWeaken 60)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_main1_three_by_three) s) <> (eval (ENum (0))
  s))%Z)) 63)::(EA 60 (AGuard (fun s => ((eval (EVar V_main1_three_by_three)
  s) = (eval (ENum (0)) s))%Z)) 61)::(EA 61 AWeaken 62)::(EA 62 ANone 65)::
  (EA 63 AWeaken 64)::(EA 64 ANone 65)::(EA 65 ANone 66)::(EA 66 ANone 68)::
  (EA 67 ANone 68)::(EA 68 ANone 69)::(EA 69 AWeaken 93)::
  (EA 70 AWeaken 71)::(EA 71 ANone 73)::(EA 71 ANone 72)::(EA 72 ANone 111)::
  (EA 73 AWeaken 74)::(EA 74 ANone 110)::(EA 74 ANone 108)::
  (EA 74 ANone 106)::(EA 74 ANone 104)::(EA 74 ANone 102)::
  (EA 74 ANone 100)::(EA 74 ANone 98)::(EA 74 ANone 96)::(EA 74 ANone 94)::
  (EA 74 ANone 84)::(EA 74 ANone 75)::(EA 75 (AAssign V_main1_argindex
  (Some (EAdd (EVar V_main1_argindex) (ENum (1))))) 76)::(EA 76 AWeaken 77)::
  (EA 77 (AGuard (fun s => ((eval (EAdd (EVar V_main1_argindex) (ENum (1)))
  s) >= (eval (EVar V_main1__tmp) s))%Z)) 81)::(EA 77 (AGuard
  (fun s => ((eval (EAdd (EVar V_main1_argindex) (ENum (1))) s) <
  (eval (EVar V_main1__tmp) s))%Z)) 78)::(EA 78 AWeaken 79)::(EA 79 (AAssign
  V_main1_bt None) 80)::(EA 80 ANone 110)::(EA 81 AWeaken 82)::
  (EA 82 ANone 83)::(EA 83 AWeaken 93)::(EA 84 (AAssign V_main1_argindex
  (Some (EAdd (EVar V_main1_argindex) (ENum (1))))) 85)::(EA 85 AWeaken 86)::
  (EA 86 (AGuard (fun s => ((eval (EAdd (EVar V_main1_argindex) (ENum (1)))
  s) >= (eval (EVar V_main1__tmp) s))%Z)) 92)::(EA 86 (AGuard
  (fun s => ((eval (EAdd (EVar V_main1_argindex) (ENum (1))) s) <
  (eval (EVar V_main1__tmp) s))%Z)) 87)::(EA 87 AWeaken 88)::
  (EA 88 ANone 89)::(EA 88 ANone 91)::(EA 89 (AAssign V_main1_three_by_three
  (Some (ENum (1)))) 90)::(EA 90 ANone 91)::(EA 91 ANone 110)::
  (EA 92 AWeaken 93)::(EA 94 (AAssign V_main1_susan_quick
  (Some (ENum (1)))) 95)::(EA 95 ANone 110)::(EA 96 (AAssign
  V_main1_three_by_three (Some (ENum (1)))) 97)::(EA 97 ANone 110)::
  (EA 98 (AAssign V_main1_drawing_mode (Some (ENum (1)))) 99)::
  (EA 99 ANone 110)::(EA 100 (AAssign V_main1_thin_post_proc
  (Some (ENum (0)))) 101)::(EA 101 ANone 110)::(EA 102 (AAssign
  V_main1_principle (Some (ENum (1)))) 103)::(EA 103 ANone 110)::
  (EA 104 (AAssign V_main1_mode (Some (ENum (2)))) 105)::(EA 105 ANone 110)::
  (EA 106 (AAssign V_main1_mode (Some (ENum (1)))) 107)::(EA 107 ANone 110)::
  (EA 108 (AAssign V_main1_mode (Some (ENum (0)))) 109)::(EA 109 ANone 110)::
  (EA 110 ANone 111)::(EA 111 (AAssign V_main1_argindex
  (Some (EAdd (EVar V_main1_argindex) (ENum (1))))) 112)::
  (EA 112 ANone 113)::(EA 113 ANone 114)::(EA 114 (AAssign V_main1_z
  (Some (EAdd (ENum (1)) (EVar V_main1_z)))) 115)::(EA 115 AWeaken 20)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_main1 => Pedges_main1
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_main1 => 93
     end)%positive;
  var_global := var_global
}.

Definition ai_main1 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 3 => (-1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 4 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0)%Z
   | 5 => (-1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0)%Z
   | 6 => (-1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0)%Z
   | 7 => (-1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0)%Z
   | 8 => (-1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_three_by_three <= 0)%Z
   | 9 => (-1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_drawing_mode <= 0)%Z
   | 10 => (-1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_susan_quick <= 0)%Z
   | 11 => (-1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0)%Z
   | 12 => (-1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 13 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 14 => (-1 * s V_main1_mode <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 15 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1__tmp + 3 <= 0)%Z
   | 16 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1__tmp + -2 <= 0)%Z
   | 17 => (1 * s V_main1__tmp + -2 <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 18 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 19 => (-1 * s V_main1_mode <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_thin_post_proc + 1 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_bt + 20 <= 0 /\ 1 * s V_main1_bt + -20 <= 0 /\ 1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_argindex + -3 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 20 => (-1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 21 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 22 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 23 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 24 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0)%Z
   | 25 => (-1 * s V_main1_principle + 1 <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 26 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ -1 * s V_main1_mode + 1 <= 0)%Z
   | 27 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ 1 * s V_main1_mode <= 0)%Z
   | 28 => (1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 29 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ 1 * s V_main1_mode + -1 <= 0 /\ -1 * s V_main1_mode + 1 <= 0)%Z
   | 30 => (-1 * s V_main1_mode + 1 <= 0 /\ 1 * s V_main1_mode + -1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 31 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 32 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 33 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 34 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0)%Z
   | 35 => (1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 36 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ 1 * s V_main1_susan_quick <= 0)%Z
   | 37 => (1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 38 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_susan_quick + 1 <= 0)%Z
   | 39 => (-1 * s V_main1_susan_quick + 1 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 40 => (-1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0)%Z
   | 41 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle + 1 <= 0)%Z
   | 42 => (-1 * s V_main1_principle + 1 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 43 => (-1 * s V_main1_principle <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 44 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 45 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 46 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0)%Z
   | 47 => (1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 48 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0)%Z
   | 49 => (1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 50 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ 1 * s V_main1_three_by_three <= 0)%Z
   | 51 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three + 1 <= 0)%Z
   | 52 => (-1 * s V_main1_three_by_three + 1 <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 53 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three + 1 <= 0)%Z
   | 54 => (-1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 55 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc <= 0 /\ -1 * s V_main1_thin_post_proc <= 0)%Z
   | 56 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0)%Z
   | 57 => (1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 58 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ 1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0)%Z
   | 59 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle + 1 <= 0)%Z
   | 60 => (-1 * s V_main1_principle + 1 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 61 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ 1 * s V_main1_three_by_three <= 0)%Z
   | 62 => (1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 63 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ -1 * s V_main1_three_by_three + 1 <= 0)%Z
   | 64 => (-1 * s V_main1_three_by_three + 1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 65 => (-1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle + 1 <= 0)%Z
   | 66 => (-1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_three_by_three <= 0)%Z
   | 67 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 68 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0)%Z
   | 69 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 70 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 71 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 72 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 73 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 74 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0)%Z
   | 75 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 76 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0)%Z
   | 77 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 78 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0)%Z
   | 79 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 80 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0)%Z
   | 81 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex + -1 <= 0)%Z
   | 82 => (1 * s V_main1__tmp+ -1 * s V_main1_argindex + -1 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 83 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex + -1 <= 0)%Z
   | 84 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 85 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0)%Z
   | 86 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 87 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0)%Z
   | 88 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 89 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0)%Z
   | 90 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_three_by_three + -1 <= 0 /\ -1 * s V_main1_three_by_three + 1 <= 0)%Z
   | 91 => (-1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 2 <= 0)%Z
   | 92 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex + -1 <= 0)%Z
   | 93 => (-1 * s V_main1_argindex + 3 <= 0 /\ 1 * s V_main1__tmp+ -1 * s V_main1_argindex + -1 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 94 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 95 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_susan_quick + -1 <= 0 /\ -1 * s V_main1_susan_quick + 1 <= 0)%Z
   | 96 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 97 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_three_by_three + -1 <= 0 /\ -1 * s V_main1_three_by_three + 1 <= 0)%Z
   | 98 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 99 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_drawing_mode + -1 <= 0 /\ -1 * s V_main1_drawing_mode + 1 <= 0)%Z
   | 100 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 101 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_thin_post_proc <= 0 /\ -1 * s V_main1_thin_post_proc <= 0)%Z
   | 102 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 103 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_principle + -1 <= 0 /\ -1 * s V_main1_principle + 1 <= 0)%Z
   | 104 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 105 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_mode + -2 <= 0 /\ -1 * s V_main1_mode + 2 <= 0)%Z
   | 106 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 107 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_mode + -1 <= 0 /\ -1 * s V_main1_mode + 1 <= 0)%Z
   | 108 => (-1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 109 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_mode <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 110 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0)%Z
   | 111 => (-1 * s V_main1__tmp+ 1 * s V_main1_argindex + 1 <= 0 /\ -1 * s V_main1_argindex + 3 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 112 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 4 <= 0)%Z
   | 113 => (-1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0)%Z
   | 114 => (-1 * s V_main1_mode <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_principle <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_argindex + 4 <= 0)%Z
   | 115 => (-1 * s V_main1_argindex + 4 <= 0 /\ -1 * s V_main1__tmp+ 1 * s V_main1_argindex <= 0 /\ -1 * s V_main1_max_no_corners + 1850 <= 0 /\ 1 * s V_main1_max_no_corners + -1850 <= 0 /\ -1 * s V_main1_drawing_mode <= 0 /\ 1 * s V_main1_thin_post_proc + -1 <= 0 /\ -1 * s V_main1_principle <= 0 /\ -1 * s V_main1_three_by_three <= 0 /\ -1 * s V_main1_susan_quick <= 0 /\ 1 * s V_main1_max_no_edges + -2650 <= 0 /\ -1 * s V_main1_max_no_edges + 2650 <= 0 /\ -1 * s V_main1_mode <= 0 /\ -1 * s V_main1_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_main1 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-2 + s V_main1_argc) <= z)%Q
   | 2 => (s V_main1_z + max0(-2 + s V_main1_argc) <= z)%Q
   | 3 => (s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 4 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 5 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 6 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 7 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 8 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 9 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 10 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 11 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 12 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 13 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 14 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 15 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 16 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 17 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 18 => (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_main1__tmp
                                             - s V_main1_argindex) (s V_main1__tmp
                                                                    - 
                                                                    s V_main1_argindex))]
     (s V_main1_z + max0(1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 20 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 21 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 22 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_main1__tmp
                                             - s V_main1_argindex) (-1
                                                                    + 
                                                                    s V_main1__tmp
                                                                    - 
                                                                    s V_main1_argindex));
      (*-1 0*) F_max0_ge_0 (-1 + s V_main1__tmp - s V_main1_argindex)]
     (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 24 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 25 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_main1__tmp
                                             - s V_main1_argindex) (-1
                                                                    + 
                                                                    s V_main1__tmp
                                                                    - 
                                                                    s V_main1_argindex));
      (*-1 0*) F_max0_ge_0 (-1 + s V_main1__tmp - s V_main1_argindex)]
     (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 27 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 28 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 29 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_main1__tmp
                                             - s V_main1_argindex) (-1
                                                                    + 
                                                                    s V_main1__tmp
                                                                    - 
                                                                    s V_main1_argindex));
      (*-1 0*) F_max0_ge_0 (-1 + s V_main1__tmp - s V_main1_argindex)]
     (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 31 => (s V_main1_z <= z)%Q
   | 32 => (s V_main1_z <= z)%Q
   | 33 => (s V_main1_z <= z)%Q
   | 34 => (s V_main1_z <= z)%Q
   | 35 => (s V_main1_z <= z)%Q
   | 36 => (s V_main1_z <= z)%Q
   | 37 => (s V_main1_z <= z)%Q
   | 38 => (s V_main1_z <= z)%Q
   | 39 => (s V_main1_z <= z)%Q
   | 40 => (s V_main1_z <= z)%Q
   | 41 => (s V_main1_z <= z)%Q
   | 42 => (s V_main1_z <= z)%Q
   | 43 => (s V_main1_z <= z)%Q
   | 44 => (s V_main1_z <= z)%Q
   | 45 => (s V_main1_z <= z)%Q
   | 46 => (s V_main1_z <= z)%Q
   | 47 => (s V_main1_z <= z)%Q
   | 48 => (s V_main1_z <= z)%Q
   | 49 => (s V_main1_z <= z)%Q
   | 50 => (s V_main1_z <= z)%Q
   | 51 => (s V_main1_z <= z)%Q
   | 52 => (s V_main1_z <= z)%Q
   | 53 => (s V_main1_z <= z)%Q
   | 54 => (s V_main1_z <= z)%Q
   | 55 => (s V_main1_z <= z)%Q
   | 56 => (s V_main1_z <= z)%Q
   | 57 => (s V_main1_z <= z)%Q
   | 58 => (s V_main1_z <= z)%Q
   | 59 => (s V_main1_z <= z)%Q
   | 60 => (s V_main1_z <= z)%Q
   | 61 => (s V_main1_z <= z)%Q
   | 62 => (s V_main1_z <= z)%Q
   | 63 => (s V_main1_z <= z)%Q
   | 64 => (s V_main1_z <= z)%Q
   | 65 => (s V_main1_z <= z)%Q
   | 66 => (s V_main1_z <= z)%Q
   | 67 => (s V_main1_z <= z)%Q
   | 68 => (s V_main1_z <= z)%Q
   | 69 => (s V_main1_z <= z)%Q
   | 70 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_main1__tmp - s V_main1_argindex) (1)]
     (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 71 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 72 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 73 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 74 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 75 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 76 => ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 77 => ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 78 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_main1__tmp - s V_main1_argindex) (1)]
     ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 79 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 80 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 81 => ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 82 => ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 83 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_main1__tmp
                                             - s V_main1_argindex) (-1
                                                                    + 
                                                                    s V_main1__tmp
                                                                    - 
                                                                    s V_main1_argindex));
      (*-1 0*) F_max0_ge_0 (-1 + s V_main1__tmp - s V_main1_argindex)]
     ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 84 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 85 => ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 86 => ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 87 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_main1__tmp - s V_main1_argindex) (1)]
     ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 88 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 89 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 90 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 91 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 92 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_main1__tmp
                                             - s V_main1_argindex) (-1
                                                                    + 
                                                                    s V_main1__tmp
                                                                    - 
                                                                    s V_main1_argindex));
      (*-1 0*) F_max0_ge_0 (-1 + s V_main1__tmp - s V_main1_argindex)]
     ((1 # 1) + s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 93 => (s V_main1_z <= z)%Q
   | 94 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 95 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 96 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 97 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 98 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 99 => ((1 # 1) + s V_main1_z
            + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 100 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 101 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 102 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 103 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 104 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 105 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 106 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 107 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 108 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 109 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 110 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 111 => ((1 # 1) + s V_main1_z
             + max0(-1 + s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 112 => ((1 # 1) + s V_main1_z
             + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 113 => ((1 # 1) + s V_main1_z
             + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 114 => ((1 # 1) + s V_main1_z
             + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | 115 => (s V_main1_z + max0(s V_main1__tmp - s V_main1_argindex) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_main1 =>
    [mkPA Q (fun n z s => ai_main1 n s /\ annot0_main1 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_main1 (proc_start P_main1) s1 (proc_end P_main1) s2 ->
    (s2 V_main1_z <= max0(-2 + s1 V_main1_argc))%Q.
Proof.
  prove_bound ipa admissible_ipa P_main1.
Qed.
