Require Import pasta.Pasta.

Inductive proc: Type :=
  P_send_tree.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_send_tree_z := 1%positive.
Notation V_send_tree__tmp := 2%positive.
Notation V_send_tree_count := 3%positive.
Notation V_send_tree_curlen := 4%positive.
Notation V_send_tree_max_count := 5%positive.
Notation V_send_tree_min_count := 6%positive.
Notation V_send_tree_n := 7%positive.
Notation V_send_tree_nextlen := 8%positive.
Notation V_send_tree_prevlen := 9%positive.
Notation V_send_tree_tree_dref_off0_off2 := 10%positive.
Notation V_send_tree_max_code := 11%positive.
Notation V_send_tree_tree := 12%positive.
Definition Pedges_send_tree: list (edge proc) :=
  (EA 1 (AAssign V_send_tree_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_send_tree__tmp (Some (EVar V_send_tree_max_code))) 3)::(EA 3 (AAssign
  V_send_tree_prevlen (Some (ENum (-1)))) 4)::(EA 4 (AAssign
  V_send_tree_nextlen (Some (EVar V_send_tree_tree_dref_off0_off2))) 5)::
  (EA 5 (AAssign V_send_tree_count (Some (ENum (0)))) 6)::(EA 6 (AAssign
  V_send_tree_max_count (Some (ENum (7)))) 7)::(EA 7 (AAssign
  V_send_tree_min_count (Some (ENum (4)))) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_send_tree_nextlen) s) =
  (eval (ENum (0)) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_send_tree_nextlen) s) <> (eval (ENum (0))
  s))%Z)) 10)::(EA 10 AWeaken 15)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_send_tree_max_count (Some (ENum (138)))) 13)::(EA 13 (AAssign
  V_send_tree_min_count (Some (ENum (3)))) 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_send_tree_n (Some (ENum (0)))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard (fun s => ((eval (EVar V_send_tree_n)
  s) <= (eval (EVar V_send_tree__tmp) s))%Z)) 21)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_send_tree_n) s) > (eval (EVar V_send_tree__tmp)
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_send_tree_curlen (Some (EVar V_send_tree_nextlen))) 23)::(EA 23 (AAssign
  V_send_tree_nextlen None) 24)::(EA 24 (AAssign V_send_tree_count
  (Some (EAdd (EVar V_send_tree_count) (ENum (1))))) 25)::
  (EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EAdd (EVar V_send_tree_count) (ENum (1))) s) <
  (eval (EVar V_send_tree_max_count) s))%Z)) 28)::(EA 26 (AGuard
  (fun s => ((eval (EAdd (EVar V_send_tree_count) (ENum (1))) s) >=
  (eval (EVar V_send_tree_max_count) s))%Z)) 27)::(EA 27 AWeaken 31)::
  (EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) =
  (eval (EVar V_send_tree_nextlen) s))%Z)) 81)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) <>
  (eval (EVar V_send_tree_nextlen) s))%Z)) 30)::(EA 30 AWeaken 31)::
  (EA 31 (AGuard (fun s => ((eval (EVar V_send_tree_count) s) <
  (eval (EVar V_send_tree_min_count) s))%Z)) 49)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_send_tree_count) s) >=
  (eval (EVar V_send_tree_min_count) s))%Z)) 32)::(EA 32 AWeaken 33)::
  (EA 33 (AGuard (fun s => ((eval (EVar V_send_tree_curlen) s) <>
  (eval (ENum (0)) s))%Z)) 41)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) = (eval (ENum (0))
  s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_send_tree_count) s) <= (eval (ENum (10))
  s))%Z)) 38)::(EA 35 (AGuard (fun s => ((eval (EVar V_send_tree_count) s) >
  (eval (ENum (10)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 ANone 40)::
  (EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 ANone 48)::
  (EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) <>
  (eval (EVar V_send_tree_prevlen) s))%Z)) 44)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) =
  (eval (EVar V_send_tree_prevlen) s))%Z)) 43)::(EA 43 AWeaken 47)::
  (EA 44 AWeaken 45)::(EA 45 (AAssign V_send_tree_count
  (Some (EAdd (EVar V_send_tree_count) (ENum (-1))))) 46)::(EA 46 ANone 47)::
  (EA 47 ANone 48)::(EA 48 ANone 57)::(EA 49 AWeaken 50)::(EA 50 ANone 51)::
  (EA 51 ANone 52)::(EA 52 (AAssign V_send_tree_count
  (Some (EAdd (EVar V_send_tree_count) (ENum (-1))))) 53)::
  (EA 53 AWeaken 54)::(EA 54 (AGuard
  (fun s => ((eval (EAdd (EVar V_send_tree_count) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 78)::(EA 54 (AGuard
  (fun s => ((eval (EAdd (EVar V_send_tree_count) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 55)::(EA 55 AWeaken 56)::(EA 56 ANone 57)::
  (EA 57 ANone 58)::(EA 58 (AAssign V_send_tree_count
  (Some (ENum (0)))) 59)::(EA 59 (AAssign V_send_tree_prevlen
  (Some (EVar V_send_tree_curlen))) 60)::(EA 60 AWeaken 61)::(EA 61 (AGuard
  (fun s => ((eval (EVar V_send_tree_nextlen) s) = (eval (ENum (0))
  s))%Z)) 73)::(EA 61 (AGuard (fun s => ((eval (EVar V_send_tree_nextlen)
  s) <> (eval (ENum (0)) s))%Z)) 62)::(EA 62 AWeaken 63)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) =
  (eval (EVar V_send_tree_nextlen) s))%Z)) 68)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_send_tree_curlen) s) <>
  (eval (EVar V_send_tree_nextlen) s))%Z)) 64)::(EA 64 AWeaken 65)::
  (EA 65 (AAssign V_send_tree_max_count (Some (ENum (7)))) 66)::
  (EA 66 (AAssign V_send_tree_min_count (Some (ENum (4)))) 67)::
  (EA 67 ANone 72)::(EA 68 AWeaken 69)::(EA 69 (AAssign V_send_tree_max_count
  (Some (ENum (6)))) 70)::(EA 70 (AAssign V_send_tree_min_count
  (Some (ENum (3)))) 71)::(EA 71 ANone 72)::(EA 72 ANone 77)::
  (EA 73 AWeaken 74)::(EA 74 (AAssign V_send_tree_max_count
  (Some (ENum (138)))) 75)::(EA 75 (AAssign V_send_tree_min_count
  (Some (ENum (3)))) 76)::(EA 76 ANone 77)::(EA 77 ANone 83)::
  (EA 78 AWeaken 79)::(EA 79 ANone 80)::(EA 80 (AAssign V_send_tree_z
  (Some (EAdd (ENum (1)) (EVar V_send_tree_z)))) 51)::(EA 81 AWeaken 82)::
  (EA 82 ANone 83)::(EA 83 (AAssign V_send_tree_n
  (Some (EAdd (EVar V_send_tree_n) (ENum (1))))) 84)::(EA 84 ANone 85)::
  (EA 85 ANone 86)::(EA 86 (AAssign V_send_tree_z (Some (EAdd (ENum (1))
  (EVar V_send_tree_z)))) 87)::(EA 87 AWeaken 18)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_send_tree => Pedges_send_tree
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_send_tree => 20
     end)%positive;
  var_global := var_global
}.

Definition ai_send_tree (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 3 => (-1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0)%Z
   | 4 => (1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0)%Z
   | 5 => (-1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0)%Z
   | 6 => (1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 7 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0)%Z
   | 8 => (-1 * s V_send_tree_max_count + 7 <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ 1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 4 <= 0)%Z
   | 9 => (-1 * s V_send_tree_min_count + 4 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0)%Z
   | 10 => (-1 * s V_send_tree_max_count + 7 <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ 1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 4 <= 0)%Z
   | 11 => (-1 * s V_send_tree_max_count + 7 <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ 1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 4 <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_nextlen <= 0)%Z
   | 12 => (-1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_min_count + 4 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0)%Z
   | 13 => (1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 4 <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_max_count + 138 <= 0)%Z
   | 14 => (-1 * s V_send_tree_max_count + 138 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_min_count + -3 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0)%Z
   | 15 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0)%Z
   | 16 => (1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_n <= 0)%Z
   | 17 => (-1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_prevlen + 1 <= 0 /\ -1 * s V_send_tree_prevlen + -1 <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0)%Z
   | 18 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 19 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree__tmp+ -1 * s V_send_tree_n + 1 <= 0)%Z
   | 20 => (1 * s V_send_tree__tmp+ -1 * s V_send_tree_n + 1 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 21 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0)%Z
   | 22 => (-1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 23 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0)%Z
   | 24 => (-1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 25 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0)%Z
   | 26 => (-1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 27 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_max_count + -1 <= 0)%Z
   | 28 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0)%Z
   | 29 => (1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 30 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0)%Z
   | 31 => (1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 32 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0)%Z
   | 33 => (-1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 34 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_curlen <= 0)%Z
   | 35 => (-1 * s V_send_tree_curlen <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 36 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_count + 11 <= 0)%Z
   | 37 => (-1 * s V_send_tree_count + 11 <= 0 /\ -1 * s V_send_tree_curlen <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 38 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_curlen <= 0 /\ 1 * s V_send_tree_count + -10 <= 0)%Z
   | 39 => (1 * s V_send_tree_count + -10 <= 0 /\ -1 * s V_send_tree_curlen <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 40 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_curlen <= 0 /\ -1 * s V_send_tree_curlen <= 0)%Z
   | 41 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0)%Z
   | 42 => (-1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 43 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_prevlen <= 0 /\ -1 * s V_send_tree_curlen+ 1 * s V_send_tree_prevlen <= 0)%Z
   | 44 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0)%Z
   | 45 => (-1 * s V_send_tree_count+ 1 * s V_send_tree_min_count <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 46 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count + -1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 47 => (1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count + -1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 48 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count+ 1 * s V_send_tree_min_count + -1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0)%Z
   | 49 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_min_count + 1 <= 0)%Z
   | 50 => (1 * s V_send_tree_count+ -1 * s V_send_tree_min_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 51 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0)%Z
   | 52 => (-1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 53 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0)%Z
   | 54 => (-1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 55 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count + -1 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0)%Z
   | 56 => (-1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count + -1 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 57 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0)%Z
   | 58 => (-1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 59 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 60 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 61 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 62 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 63 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 64 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 65 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 66 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ -1 * s V_send_tree_max_count + 7 <= 0)%Z
   | 67 => (-1 * s V_send_tree_max_count + 7 <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 4 <= 0)%Z
   | 68 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_curlen+ 1 * s V_send_tree_nextlen <= 0)%Z
   | 69 => (-1 * s V_send_tree_curlen+ 1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 70 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_curlen+ 1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_max_count + -6 <= 0 /\ -1 * s V_send_tree_max_count + 6 <= 0)%Z
   | 71 => (-1 * s V_send_tree_max_count + 6 <= 0 /\ 1 * s V_send_tree_max_count + -6 <= 0 /\ -1 * s V_send_tree_curlen+ 1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -3 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0)%Z
   | 72 => (1 * s V_send_tree_min_count + -4 <= 0 /\ 1 * s V_send_tree_max_count + -7 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_max_count + 6 <= 0)%Z
   | 73 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_nextlen <= 0)%Z
   | 74 => (-1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0)%Z
   | 75 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_max_count + 138 <= 0)%Z
   | 76 => (-1 * s V_send_tree_max_count + 138 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -3 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0)%Z
   | 77 => (-1 * s V_send_tree_max_count + 6 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0)%Z
   | 78 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0)%Z
   | 79 => (-1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0)%Z
   | 80 => (-1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_n <= 0)%Z
   | 81 => (1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_nextlen <= 0 /\ -1 * s V_send_tree_curlen+ 1 * s V_send_tree_nextlen <= 0)%Z
   | 82 => (-1 * s V_send_tree_curlen+ 1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_curlen+ -1 * s V_send_tree_nextlen <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_count + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0)%Z
   | 83 => (-1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_n <= 0 /\ -1 * s V_send_tree_z <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0)%Z
   | 84 => (1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n + -1 <= 0)%Z
   | 85 => (-1 * s V_send_tree__tmp+ 1 * s V_send_tree_n + -1 <= 0 /\ -1 * s V_send_tree_n + 1 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0)%Z
   | 86 => (1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_z <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ -1 * s V_send_tree_n + 1 <= 0 /\ -1 * s V_send_tree__tmp+ 1 * s V_send_tree_n + -1 <= 0)%Z
   | 87 => (-1 * s V_send_tree__tmp+ 1 * s V_send_tree_n + -1 <= 0 /\ -1 * s V_send_tree_n + 1 <= 0 /\ -1 * s V_send_tree_count <= 0 /\ 1 * s V_send_tree_min_count + -4 <= 0 /\ -1 * s V_send_tree_min_count + 3 <= 0 /\ 1 * s V_send_tree_max_count + -138 <= 0 /\ 1 * s V_send_tree_count+ -1 * s V_send_tree_max_count + 2 <= 0 /\ -1 * s V_send_tree_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_send_tree (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 3) * max0(1 + s V_send_tree_max_code) <= z)%Q
   | 2 => (s V_send_tree_z + (4 # 3) * max0(1 + s V_send_tree_max_code) <= z)%Q
   | 3 => (s V_send_tree_z + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 4 => (s V_send_tree_z + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 5 => (s V_send_tree_z + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 6 => ((1 # 3) + s V_send_tree_z
           + (1 # 135) * max0(-5 + s V_send_tree_count)
           + (1 # 135) * max0(-4 + s V_send_tree_count)
           + (1 # 135) * max0(-1 + s V_send_tree_count)
           + (4 # 3) * max0(1 + s V_send_tree__tmp)
           - (1 # 3) * max0(1 - s V_send_tree_count) <= z)%Q
   | 7 => ((1 # 2) - (2 # 85) * s V_send_tree_max_count + s V_send_tree_z
           + (2 # 85) * max0(-7 + s V_send_tree_max_count)
           + (1 # 135) * max0(-5 + s V_send_tree_count)
           + (1 # 135) * max0(-4 + s V_send_tree_count)
           + (1 # 135) * max0(-1 + s V_send_tree_count)
           + (4 # 3) * max0(1 + s V_send_tree__tmp)
           - (1 # 3) * max0(1 - s V_send_tree_count) <= z)%Q
   | 8 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_send_tree_count) (0))) (F_max0_ge_0 (1
                                                                    - s V_send_tree_count));
      (*-0.00735294 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                          + s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.00735294 0*) F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                          + s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.00735294 0*) F_binom_monotonic 1 (F_max0_ge_0 (-5
                                                          + s V_send_tree_count)) (F_check_ge (0) (0))]
     ((1 # 2) - (2 # 85) * s V_send_tree_max_count + s V_send_tree_z
      + (2 # 85) * max0(-7 + s V_send_tree_max_count)
      + (1 # 135) * max0(-5 + s V_send_tree_count)
      + (1 # 135) * max0(-4 + s V_send_tree_count)
      + (1 # 135) * max0(-1 + s V_send_tree_count)
      + (4 # 3) * max0(1 + s V_send_tree__tmp)
      - (1 # 3) * max0(1 - s V_send_tree_count) <= z)%Q
   | 9 => ((12 # 73) + (1 # 3) * s V_send_tree_count
           - (2 # 85) * s V_send_tree_max_count + s V_send_tree_z
           + (2 # 85) * max0(-7 + s V_send_tree_max_count)
           + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 10 => hints
     [(*-0.0234846 0*) F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                           + s V_send_tree_max_count)) (F_check_ge (-7
                                                                    + s V_send_tree_max_count) (0))]
     ((12 # 73) + (1 # 3) * s V_send_tree_count
      - (2 # 85) * s V_send_tree_max_count + s V_send_tree_z
      + (2 # 85) * max0(-7 + s V_send_tree_max_count)
      + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 11 => hints
     [(*0 0.0234846*) F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                          + s V_send_tree_max_count)) (F_check_ge (-7
                                                                    + s V_send_tree_max_count) (0))]
     ((12 # 73) + (1 # 3) * s V_send_tree_count
      - (2 # 85) * s V_send_tree_max_count + s V_send_tree_z
      + (2 # 85) * max0(-7 + s V_send_tree_max_count)
      + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 12 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 13 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 14 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 15 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp) <= z)%Q
   | 16 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n) <= z)%Q
   | 17 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n) <= z)%Q
   | 18 => ((1 # 3) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n) <= z)%Q
   | 19 => hints
     [(*-1.33333 0*) F_max0_monotonic (F_check_ge (1 + s V_send_tree__tmp
                                                   - s V_send_tree_n) (s V_send_tree__tmp
                                                                    - s V_send_tree_n));
      (*-1.33333 0*) F_max0_ge_0 (s V_send_tree__tmp - s V_send_tree_n);
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_send_tree_count) (0))) (F_max0_ge_0 (s V_send_tree_count))]
     ((1 # 3) * s V_send_tree_count + s V_send_tree_z
      + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n) <= z)%Q
   | 20 => (s V_send_tree_z <= z)%Q
   | 21 => hints
     [(*0 0.00729927*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (136
                                                                    - s V_send_tree_count) (0))) (F_max0_ge_0 (136
                                                                    - s V_send_tree_count))]
     ((1 # 3) * s V_send_tree_count + s V_send_tree_z
      + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n) <= z)%Q
   | 22 => (-(135 # 136) + (31 # 91) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n)
            + (1 # 136) * max0(136 - s V_send_tree_count) <= z)%Q
   | 23 => (-(135 # 136) + (31 # 91) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n)
            + (1 # 136) * max0(136 - s V_send_tree_count) <= z)%Q
   | 24 => (-(135 # 136) + (31 # 91) * s V_send_tree_count + s V_send_tree_z
            + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n)
            + (1 # 136) * max0(136 - s V_send_tree_count) <= z)%Q
   | 25 => hints
     [(*-1.33333 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                         + s V_send_tree__tmp
                                                         - s V_send_tree_n)) (F_check_ge (1
                                                                    + s V_send_tree__tmp
                                                                    - s V_send_tree_n) (0));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))) (F_max0_ge_0 (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count))]
     (-(4 # 3) + (31 # 91) * s V_send_tree_count + s V_send_tree_z
      + (4 # 3) * max0(1 + s V_send_tree__tmp - s V_send_tree_n)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 26 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 27 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 28 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 29 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 30 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 31 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 32 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 33 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 34 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 35 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (44 # 119) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 36 => hints
     [(*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_0 (138
                                                         - s V_send_tree_max_count)) (F_check_ge (0) (0));
      (*-0.00729927 0*) F_binom_monotonic 1 (F_max0_ge_arg (137
                                                            - s V_send_tree_count)) (F_check_ge (137
                                                                    - s V_send_tree_count) (0));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         - s V_send_tree_count
                                                         + s V_send_tree_max_count)) (F_check_ge (0) (0));
      (*-0.362434 0*) F_binom_monotonic 1 (F_max0_ge_0 (-11
                                                        + s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.362434 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-11
                                                                    + s V_send_tree_count) (0))) (F_max0_ge_0 (-11
                                                                    + s V_send_tree_count))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (44 # 119) * s V_send_tree_count
      - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 37 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 38 => hints
     [(*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (138
                                                           - s V_send_tree_max_count)) (F_check_ge (138
                                                                    - s V_send_tree_max_count) (0));
      (*-0.00729927 0*) F_binom_monotonic 1 (F_max0_ge_arg (137
                                                            - s V_send_tree_count)) (F_check_ge (137
                                                                    - s V_send_tree_count) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                        + s V_send_tree_count
                                                        - s V_send_tree_min_count)) (F_check_ge (0) (0));
      (*0 0.333333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + 
                                                                    s V_send_tree_count
                                                                    - 
                                                                    s V_send_tree_min_count) (0))) (F_max0_ge_0 (1
                                                                    + s V_send_tree_count
                                                                    - s V_send_tree_min_count));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                        + s V_send_tree_min_count)) (F_check_ge (0) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_send_tree_min_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_send_tree_min_count));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           - s V_send_tree_count
                                                           + s V_send_tree_max_count)) (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (44 # 119) * s V_send_tree_count
      - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 39 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 40 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 41 => hints
     [(*0 0.335784*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_send_tree_count
                                                                    - 
                                                                    s V_send_tree_min_count) (0))) (F_max0_ge_0 (s V_send_tree_count
                                                                    - s V_send_tree_min_count))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (44 # 119) * s V_send_tree_count
      - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 42 => ((3 # 103) + (4 # 3) * s V_send_tree__tmp
            + (2 # 59) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count
            + (44 # 131) * s V_send_tree_min_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z
            + (3 # 103) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count)
            + (44 # 131) * max0(s V_send_tree_count - s V_send_tree_min_count) <= z)%Q
   | 43 => hints
     [(*-0.335784 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_send_tree_count
                                                        - s V_send_tree_min_count)) (F_check_ge (0) (0));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (138
                                                           - s V_send_tree_max_count)) (F_check_ge (138
                                                                    - s V_send_tree_max_count) (0));
      (*-0.00245098 0*) F_binom_monotonic 1 (F_max0_ge_0 (137
                                                          - s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.00484829 0*) F_binom_monotonic 1 (F_max0_ge_arg (137
                                                            - s V_send_tree_count)) (F_check_ge (137
                                                                    - s V_send_tree_count) (0));
      (*-0.335784 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                        + s V_send_tree_min_count)) (F_check_ge (0) (0));
      (*-0.335784 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_send_tree_min_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_send_tree_min_count));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           - s V_send_tree_count
                                                           + s V_send_tree_max_count)) (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (2 # 59) * s V_send_tree_count - (3 # 103) * s V_send_tree_max_count
      + (44 # 131) * s V_send_tree_min_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count)
      + (44 # 131) * max0(s V_send_tree_count - s V_send_tree_min_count) <= z)%Q
   | 44 => hints
     [(*-0.335784 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_send_tree_count
                                                        - s V_send_tree_min_count)) (F_check_ge (0) (0));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (138
                                                           - s V_send_tree_max_count)) (F_check_ge (138
                                                                    - s V_send_tree_max_count) (0));
      (*-0.00245098 0*) F_binom_monotonic 1 (F_max0_ge_0 (137
                                                          - s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.00484829 0*) F_binom_monotonic 1 (F_max0_ge_arg (137
                                                            - s V_send_tree_count)) (F_check_ge (137
                                                                    - s V_send_tree_count) (0));
      (*-0.335784 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                        + s V_send_tree_min_count)) (F_check_ge (0) (0));
      (*-0.335784 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_send_tree_min_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_send_tree_min_count));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           - s V_send_tree_count
                                                           + s V_send_tree_max_count)) (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (2 # 59) * s V_send_tree_count - (3 # 103) * s V_send_tree_max_count
      + (44 # 131) * s V_send_tree_min_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count)
      + (44 # 131) * max0(s V_send_tree_count - s V_send_tree_min_count) <= z)%Q
   | 45 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 46 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 47 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 48 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 49 => hints
     [(*0 0.00729927*) F_binom_monotonic 1 (F_max0_ge_arg (137
                                                           - s V_send_tree_count)) (F_check_ge (137
                                                                    - s V_send_tree_count) (0));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                        - s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - s V_send_tree_count) (0))) (F_max0_ge_0 (3
                                                                    - s V_send_tree_count));
      (*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           - s V_send_tree_count
                                                           + s V_send_tree_max_count)) (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (44 # 119) * s V_send_tree_count
      - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 50 => (-(1 # 1) + (4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 51 => (-(1 # 1) + (4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 52 => (-(1 # 1) + (4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 53 => ((4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 54 => ((4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 55 => hints
     [(*-0.0291005 0*) F_binom_monotonic 1 (F_max0_ge_arg (138
                                                           - s V_send_tree_max_count)) (F_check_ge (138
                                                                    - s V_send_tree_max_count) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_send_tree_count)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_send_tree_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_send_tree_count))]
     ((4 # 3) * s V_send_tree__tmp + s V_send_tree_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 56 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 57 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 58 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 59 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z + (1 # 135) * max0(-3 + s V_send_tree_count)
            + (1 # 135) * max0(-2 + s V_send_tree_count)
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 60 => hints
     [(*-0.00735294 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                          + s V_send_tree_count)) (F_check_ge (0) (0));
      (*-0.00735294 0*) F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                          + s V_send_tree_count)) (F_check_ge (0) (0))]
     ((316 # 63) + (4 # 3) * s V_send_tree__tmp
      + (1 # 3) * s V_send_tree_count - (3 # 103) * s V_send_tree_max_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z
      + (1 # 135) * max0(-3 + s V_send_tree_count)
      + (1 # 135) * max0(-2 + s V_send_tree_count)
      - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 61 => ((316 # 63) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 62 => hints
     [(*0 0.0291005*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (138
                                                                    - s V_send_tree_max_count) (0))) (F_max0_ge_0 (138
                                                                    - s V_send_tree_max_count));
      (*0 0.00729927*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (137
                                                                    - s V_send_tree_count) (0))) (F_max0_ge_0 (137
                                                                    - s V_send_tree_count))]
     ((316 # 63) + (4 # 3) * s V_send_tree__tmp
      + (1 # 3) * s V_send_tree_count - (3 # 103) * s V_send_tree_max_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z
      - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 63 => ((4 # 3) * s V_send_tree__tmp + (31 # 91) * s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 64 => hints
     [(*-0.00729927 0*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                            - s V_send_tree_count)) (F_check_ge (6
                                                                    - s V_send_tree_count) (0))]
     ((4 # 3) * s V_send_tree__tmp + (31 # 91) * s V_send_tree_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 65 => ((5 # 114) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z - (1 # 136) * max0(6 - s V_send_tree_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 66 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 67 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 68 => hints
     [(*-0.00729927 0*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                            - s V_send_tree_count)) (F_check_ge (5
                                                                    - s V_send_tree_count) (0))]
     ((4 # 3) * s V_send_tree__tmp + (31 # 91) * s V_send_tree_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 69 => ((3 # 82) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z - (1 # 136) * max0(5 - s V_send_tree_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 70 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 71 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 72 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 73 => hints
     [(*-0.0291005 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (138
                                                                    - s V_send_tree_max_count) (0))) (F_max0_ge_0 (138
                                                                    - s V_send_tree_max_count))]
     ((316 # 63) + (4 # 3) * s V_send_tree__tmp
      + (1 # 3) * s V_send_tree_count - (3 # 103) * s V_send_tree_max_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z
      - (3 # 103) * max0(138 - s V_send_tree_max_count) <= z)%Q
   | 74 => ((1 # 1) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z <= z)%Q
   | 75 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 76 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 77 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 78 => ((4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 79 => ((4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 80 => ((4 # 3) * s V_send_tree__tmp + s V_send_tree_count
            - (4 # 3) * s V_send_tree_n + s V_send_tree_z <= z)%Q
   | 81 => hints
     [(*-0.0363998 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           - s V_send_tree_count
                                                           + s V_send_tree_max_count)) (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))]
     ((3 # 103) + (4 # 3) * s V_send_tree__tmp
      + (44 # 119) * s V_send_tree_count
      - (3 # 103) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
      + s V_send_tree_z
      + (3 # 103) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 82 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 83 => (-(1 # 136) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 84 => ((61 # 46) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 85 => ((61 # 46) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 86 => ((61 # 46) + (4 # 3) * s V_send_tree__tmp
            + (1 # 3) * s V_send_tree_count
            + (1 # 136) * s V_send_tree_max_count - (4 # 3) * s V_send_tree_n
            + s V_send_tree_z
            - (1 # 136) * max0(-1 - s V_send_tree_count
                               + s V_send_tree_max_count)
            + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | 87 => hints
     [(*-0.00729927 0*) F_binom_monotonic 1 (F_max0_ge_arg (137
                                                            - s V_send_tree_count)) (F_check_ge (137
                                                                    - s V_send_tree_count) (0));
      (*0 1.33333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + 
                                                                    s V_send_tree__tmp
                                                                    - 
                                                                    s V_send_tree_n) (0))) (F_max0_ge_0 (1
                                                                    + s V_send_tree__tmp
                                                                    - s V_send_tree_n));
      (*-0.00729927 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count) (0))) (F_max0_ge_0 (-1
                                                                    - s V_send_tree_count
                                                                    + s V_send_tree_max_count))]
     ((15 # 46) + (4 # 3) * s V_send_tree__tmp
      + (1 # 3) * s V_send_tree_count + (1 # 136) * s V_send_tree_max_count
      - (4 # 3) * s V_send_tree_n + s V_send_tree_z
      - (1 # 136) * max0(-1 - s V_send_tree_count + s V_send_tree_max_count)
      + (1 # 136) * max0(137 - s V_send_tree_count) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_send_tree =>
    [mkPA Q (fun n z s => ai_send_tree n s /\ annot0_send_tree n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_send_tree (proc_start P_send_tree) s1 (proc_end P_send_tree) s2 ->
    (s2 V_send_tree_z <= (4 # 3) * max0(1 + s1 V_send_tree_max_code))%Q.
Proof.
  prove_bound ipa admissible_ipa P_send_tree.
Qed.
