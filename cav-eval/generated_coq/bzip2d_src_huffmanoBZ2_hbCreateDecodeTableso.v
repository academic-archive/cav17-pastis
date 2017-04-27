Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BZ2_hbCreateDecodeTables.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BZ2_hbCreateDecodeTables_z := 1%positive.
Notation V_BZ2_hbCreateDecodeTables__tmp := 2%positive.
Notation V_BZ2_hbCreateDecodeTables__tmp1 := 3%positive.
Notation V_BZ2_hbCreateDecodeTables__tmp2 := 4%positive.
Notation V_BZ2_hbCreateDecodeTables_i := 5%positive.
Notation V_BZ2_hbCreateDecodeTables_j := 6%positive.
Notation V_BZ2_hbCreateDecodeTables_pp := 7%positive.
Notation V_BZ2_hbCreateDecodeTables_vec := 8%positive.
Notation V_BZ2_hbCreateDecodeTables_alphaSize := 9%positive.
Notation V_BZ2_hbCreateDecodeTables_base := 10%positive.
Notation V_BZ2_hbCreateDecodeTables_length := 11%positive.
Notation V_BZ2_hbCreateDecodeTables_limit := 12%positive.
Notation V_BZ2_hbCreateDecodeTables_maxLen := 13%positive.
Notation V_BZ2_hbCreateDecodeTables_minLen := 14%positive.
Notation V_BZ2_hbCreateDecodeTables_perm := 15%positive.
Definition Pedges_BZ2_hbCreateDecodeTables: list (edge proc) :=
  (EA 1 (AAssign V_BZ2_hbCreateDecodeTables_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_BZ2_hbCreateDecodeTables__tmp2
  (Some (EVar V_BZ2_hbCreateDecodeTables_minLen))) 3)::(EA 3 (AAssign
  V_BZ2_hbCreateDecodeTables__tmp
  (Some (EVar V_BZ2_hbCreateDecodeTables_maxLen))) 4)::(EA 4 (AAssign
  V_BZ2_hbCreateDecodeTables__tmp1
  (Some (EVar V_BZ2_hbCreateDecodeTables_alphaSize))) 5)::(EA 5 (AAssign
  V_BZ2_hbCreateDecodeTables_pp (Some (ENum (0)))) 6)::(EA 6 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EVar V_BZ2_hbCreateDecodeTables__tmp2))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) <=
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp) s))%Z)) 87)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp) s))%Z)) 10)::
  (EA 10 AWeaken 11)::(EA 11 (AAssign V_BZ2_hbCreateDecodeTables_i
  (Some (ENum (0)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) <
  (eval (ENum (23)) s))%Z)) 80)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >=
  (eval (ENum (23)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_BZ2_hbCreateDecodeTables_i (Some (ENum (0)))) 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) <
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp1) s))%Z)) 73)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >=
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp1) s))%Z)) 20)::
  (EA 20 AWeaken 21)::(EA 21 (AAssign V_BZ2_hbCreateDecodeTables_i
  (Some (ENum (1)))) 22)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::
  (EA 24 (AGuard (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) <
  (eval (ENum (23)) s))%Z)) 66)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >=
  (eval (ENum (23)) s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 (AAssign
  V_BZ2_hbCreateDecodeTables_i (Some (ENum (0)))) 27)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) < (eval (ENum (23))
  s))%Z)) 59)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >=
  (eval (ENum (23)) s))%Z)) 30)::(EA 30 AWeaken 31)::(EA 31 (AAssign
  V_BZ2_hbCreateDecodeTables_vec (Some (ENum (0)))) 32)::(EA 32 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EVar V_BZ2_hbCreateDecodeTables__tmp2))) 33)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) <=
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp) s))%Z)) 50)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp) s))%Z)) 36)::
  (EA 36 AWeaken 37)::(EA 37 (AAssign V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables__tmp2) (ENum (1))))) 38)::
  (EA 38 ANone 39)::(EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) <=
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp) s))%Z)) 43)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_i) s) >
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp) s))%Z)) 41)::
  (EA 41 AWeaken 42)::(EA 43 AWeaken 44)::(EA 44 ANone 45)::(EA 45 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 46)::
  (EA 46 ANone 47)::(EA 47 ANone 48)::(EA 48 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 49)::(EA 49 AWeaken 40)::
  (EA 50 AWeaken 51)::(EA 51 (AAssign V_BZ2_hbCreateDecodeTables_vec
  None) 52)::(EA 52 (AAssign V_BZ2_hbCreateDecodeTables_vec None) 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 55)::
  (EA 55 ANone 56)::(EA 56 ANone 57)::(EA 57 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 58)::(EA 58 AWeaken 35)::
  (EA 59 AWeaken 60)::(EA 60 ANone 61)::(EA 61 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 62)::
  (EA 62 ANone 63)::(EA 63 ANone 64)::(EA 64 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 65)::(EA 65 AWeaken 29)::
  (EA 66 AWeaken 67)::(EA 67 ANone 68)::(EA 68 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 69)::
  (EA 69 ANone 70)::(EA 70 ANone 71)::(EA 71 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 72)::(EA 72 AWeaken 24)::
  (EA 73 AWeaken 74)::(EA 74 ANone 75)::(EA 75 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 76)::
  (EA 76 ANone 77)::(EA 77 ANone 78)::(EA 78 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 79)::(EA 79 AWeaken 19)::
  (EA 80 AWeaken 81)::(EA 81 ANone 82)::(EA 82 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 83)::
  (EA 83 ANone 84)::(EA 84 ANone 85)::(EA 85 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 86)::(EA 86 AWeaken 14)::
  (EA 87 AWeaken 88)::(EA 88 (AAssign V_BZ2_hbCreateDecodeTables_j
  (Some (ENum (0)))) 89)::(EA 89 ANone 90)::(EA 90 AWeaken 91)::
  (EA 91 (AGuard (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_j) s) <
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp1) s))%Z)) 99)::(EA 91 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbCreateDecodeTables_j) s) >=
  (eval (EVar V_BZ2_hbCreateDecodeTables__tmp1) s))%Z)) 92)::
  (EA 92 AWeaken 93)::(EA 93 ANone 94)::(EA 94 (AAssign
  V_BZ2_hbCreateDecodeTables_i
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_i) (ENum (1))))) 95)::
  (EA 95 ANone 96)::(EA 96 ANone 97)::(EA 97 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 98)::(EA 98 AWeaken 9)::
  (EA 99 AWeaken 100)::(EA 100 ANone 101)::(EA 100 ANone 103)::
  (EA 101 (AAssign V_BZ2_hbCreateDecodeTables_pp
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_pp) (ENum (1))))) 102)::
  (EA 102 ANone 103)::(EA 103 ANone 104)::(EA 104 (AAssign
  V_BZ2_hbCreateDecodeTables_j
  (Some (EAdd (EVar V_BZ2_hbCreateDecodeTables_j) (ENum (1))))) 105)::
  (EA 105 ANone 106)::(EA 106 ANone 107)::(EA 107 (AAssign
  V_BZ2_hbCreateDecodeTables_z (Some (EAdd (ENum (1))
  (EVar V_BZ2_hbCreateDecodeTables_z)))) 108)::(EA 108 AWeaken 91)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BZ2_hbCreateDecodeTables => Pedges_BZ2_hbCreateDecodeTables
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BZ2_hbCreateDecodeTables => 42
     end)%positive;
  var_global := var_global
}.

Definition ai_BZ2_hbCreateDecodeTables (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 3 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 4 => (1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 5 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 6 => (1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 7 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 8 => (1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 9 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 10 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp+ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0)%Z
   | 11 => (1 * s V_BZ2_hbCreateDecodeTables__tmp+ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 12 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 13 => (-1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 14 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 15 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0)%Z
   | 16 => (-1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 17 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 18 => (-1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 19 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 20 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 21 => (1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 22 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0)%Z
   | 23 => (-1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 24 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 25 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0)%Z
   | 26 => (-1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 27 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 28 => (-1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 29 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 30 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0)%Z
   | 31 => (-1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 32 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 23 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_vec <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_vec <= 0)%Z
   | 33 => (-1 * s V_BZ2_hbCreateDecodeTables_vec <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_vec <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 34 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_vec <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_vec <= 0)%Z
   | 35 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 36 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp+ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0)%Z
   | 37 => (1 * s V_BZ2_hbCreateDecodeTables__tmp+ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 38 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 39 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 40 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 41 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp+ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0)%Z
   | 42 => (1 * s V_BZ2_hbCreateDecodeTables__tmp+ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 43 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 44 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 45 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 46 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0)%Z
   | 47 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 48 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0)%Z
   | 49 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 50 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 51 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 52 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 53 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 54 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 55 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0)%Z
   | 56 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 57 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0)%Z
   | 58 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 59 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0)%Z
   | 60 => (1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 61 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0)%Z
   | 62 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 63 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 64 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 65 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 66 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0)%Z
   | 67 => (1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 68 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0)%Z
   | 69 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 2 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 70 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 2 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 71 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 2 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 72 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 2 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 73 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0)%Z
   | 74 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 75 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0)%Z
   | 76 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 77 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 78 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 79 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 80 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0)%Z
   | 81 => (1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 82 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -22 <= 0)%Z
   | 83 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 84 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0)%Z
   | 85 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0)%Z
   | 86 => (1 * s V_BZ2_hbCreateDecodeTables_i + -23 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_i + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 87 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 88 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 89 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0)%Z
   | 90 => (-1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 91 => (-1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 92 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0)%Z
   | 93 => (1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 94 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0)%Z
   | 95 => (1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0)%Z
   | 96 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0)%Z
   | 97 => (1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0)%Z
   | 98 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i + -1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ 1 * s V_BZ2_hbCreateDecodeTables__tmp1+ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | 99 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0)%Z
   | 100 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0)%Z
   | 101 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0)%Z
   | 102 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp + 1 <= 0)%Z
   | 103 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0)%Z
   | 104 => (-1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 105 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0)%Z
   | 106 => (-1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0)%Z
   | 107 => (-1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0)%Z
   | 108 => (-1 * s V_BZ2_hbCreateDecodeTables_j + 1 <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp1+ 1 * s V_BZ2_hbCreateDecodeTables_j <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables__tmp+ 1 * s V_BZ2_hbCreateDecodeTables_i <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_pp <= 0 /\ -1 * s V_BZ2_hbCreateDecodeTables_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BZ2_hbCreateDecodeTables (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((68 # 1)
           + (2 # 1) * max0(1 + s V_BZ2_hbCreateDecodeTables_maxLen
                            - s V_BZ2_hbCreateDecodeTables_minLen)
           + max0(1 + s V_BZ2_hbCreateDecodeTables_maxLen
                  - s V_BZ2_hbCreateDecodeTables_minLen) * max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables_maxLen
                  - s V_BZ2_hbCreateDecodeTables_minLen) <= z)%Q
   | 2 => ((68 # 1)
           + (2 # 1) * max0(1 + s V_BZ2_hbCreateDecodeTables_maxLen
                            - s V_BZ2_hbCreateDecodeTables_minLen)
           + max0(1 + s V_BZ2_hbCreateDecodeTables_maxLen
                  - s V_BZ2_hbCreateDecodeTables_minLen) * max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables_maxLen
                  - s V_BZ2_hbCreateDecodeTables_minLen)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 3 => ((68 # 1)
           + (2 # 1) * max0(1 - s V_BZ2_hbCreateDecodeTables__tmp2
                            + s V_BZ2_hbCreateDecodeTables_maxLen)
           + max0(1 - s V_BZ2_hbCreateDecodeTables__tmp2
                  + s V_BZ2_hbCreateDecodeTables_maxLen) * max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(-s V_BZ2_hbCreateDecodeTables__tmp2
                  + s V_BZ2_hbCreateDecodeTables_maxLen)
           + max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 4 => ((68 # 1)
           + (2 # 1) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2) * max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(s V_BZ2_hbCreateDecodeTables_alphaSize)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 5 => ((68 # 1)
           + (2 # 1) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 6 => ((68 # 1)
           + (2 # 1) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 7 => ((68 # 1)
           + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
           - s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
           + s V_BZ2_hbCreateDecodeTables__tmp2
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
           - s V_BZ2_hbCreateDecodeTables_i
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                             - s V_BZ2_hbCreateDecodeTables__tmp
                                                             + s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                             + s V_BZ2_hbCreateDecodeTables__tmp
                                                             - s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
           - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables_i)
           - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
           + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables_i)
           + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 8 => ((68 # 1)
           + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
           - s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
           + s V_BZ2_hbCreateDecodeTables__tmp2
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
           - s V_BZ2_hbCreateDecodeTables_i
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                             - s V_BZ2_hbCreateDecodeTables__tmp
                                                             + s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                             + s V_BZ2_hbCreateDecodeTables__tmp
                                                             - s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
           - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables_i)
           - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
           + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables_i)
           + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 9 => ((68 # 1)
           + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
           - s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
           + s V_BZ2_hbCreateDecodeTables__tmp2
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
           - s V_BZ2_hbCreateDecodeTables_i
           - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                             - s V_BZ2_hbCreateDecodeTables__tmp
                                                             + s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                             + s V_BZ2_hbCreateDecodeTables__tmp
                                                             - s V_BZ2_hbCreateDecodeTables_i)
           + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
           - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables_i)
           - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                            + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
           + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables__tmp2)
           + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables_i)
           + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp
                  - s V_BZ2_hbCreateDecodeTables__tmp2)
           + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
           + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                             + s V_BZ2_hbCreateDecodeTables__tmp
                                             - s V_BZ2_hbCreateDecodeTables_i) (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-1 0*) F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables_i);
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i))]
     ((68 # 1)
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 11 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 12 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 23) * s V_BZ2_hbCreateDecodeTables_z * max0(23
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 23) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 13 => hints
     [(*-0.0434783 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)))]
     ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 23) * s V_BZ2_hbCreateDecodeTables_z * max0(23
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 23) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_z)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 14 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (23
                                             - s V_BZ2_hbCreateDecodeTables_i) (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-1 0*) F_max0_ge_0 (22 - s V_BZ2_hbCreateDecodeTables_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (23
                                                              - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i))]
     ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 16 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 17 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 18 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 19 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                             - s V_BZ2_hbCreateDecodeTables_i) (-1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                            - s V_BZ2_hbCreateDecodeTables_i)]
     ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1
             - s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 21 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 22 => ((9319 # 196) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (271 # 131) * s V_BZ2_hbCreateDecodeTables_i
            - (7 # 162) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                                + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 22) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (109 # 112) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            - (7 # 162) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 89) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 11) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 23 => hints
     [(*-0.0227273 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))]
     ((9319 # 196) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (271 # 131) * s V_BZ2_hbCreateDecodeTables_i
      - (7 # 162) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                          + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 22) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 44) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (109 # 112) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      - (7 # 162) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(23
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 89) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 11) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 24 => ((9319 # 196) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (178 # 87) * s V_BZ2_hbCreateDecodeTables_i
            - (7 # 162) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                                + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 22) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (109 # 112) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            - (7 # 162) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 89) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 11) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 25 => hints
     [(*-0.0454545 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i));
      (*-0.0454545 0*) F_binom_monotonic 2 (F_max0_ge_0 (-23
                                                         + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0));
      (*-0.0225391 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                           + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0));
      (*-0.0227273 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0));
      (*-0.0454545 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-2
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0454545 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.04329 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)))]
     ((9319 # 196) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (178 # 87) * s V_BZ2_hbCreateDecodeTables_i
      - (7 # 162) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                          + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 22) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 44) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (109 # 112) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      - (7 # 162) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(23
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 89) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 11) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 26 => ((45 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 27 => ((2566 # 117) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (191 # 100) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 74) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (23 # 48) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 46) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 46) * max0(1 - s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 101) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (0 # 1) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 28 => hints
     [(*-0.0217822 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)))]
     ((2566 # 117) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (191 # 100) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                         + s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 74) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (23 # 48) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 46) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
      - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 46) * max0(1 - s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 101) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (0 # 1) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 29 => ((2566 # 117) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (191 # 100) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 74) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (23 # 48) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 46) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 46) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 101) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (0 # 1) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 30 => hints
     [(*-0.0437294 0*) F_max0_monotonic (F_check_ge (23
                                                     - s V_BZ2_hbCreateDecodeTables_i) (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-0.0437294 0*) F_max0_ge_0 (22 - s V_BZ2_hbCreateDecodeTables_i);
      (*-0.0406371 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i));
      (*-0.039769 0*) F_binom_monotonic 2 (F_max0_ge_0 (-23
                                                        + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0));
      (*-0.0019802 0*) F_binom_monotonic 2 (F_max0_ge_0 (23
                                                         - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0));
      (*-0.019967 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.000868131 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0208351 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0208351 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0217822 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0417492 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.000947052 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0417492 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0199221 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.019967 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-23
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.039769 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z))]
     ((2566 # 117) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (191 # 100) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                         + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 74) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (23 # 48) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 46) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
      - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 46) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 101) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (0 # 1) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 31 => (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 32 => (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 33 => (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              - s V_BZ2_hbCreateDecodeTables__tmp
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 34 => (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              - s V_BZ2_hbCreateDecodeTables__tmp
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 35 => (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              - s V_BZ2_hbCreateDecodeTables__tmp
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                             + s V_BZ2_hbCreateDecodeTables__tmp
                                             - s V_BZ2_hbCreateDecodeTables_i) (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-1 0*) F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables_i);
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                     - s V_BZ2_hbCreateDecodeTables__tmp
                                                     + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                - s V_BZ2_hbCreateDecodeTables__tmp
                                                                + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     - s V_BZ2_hbCreateDecodeTables__tmp
                                                     + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))]
     (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables_i
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 37 => (max0(s V_BZ2_hbCreateDecodeTables__tmp
                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 38 => (max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 39 => (max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 40 => (max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                             + s V_BZ2_hbCreateDecodeTables__tmp
                                             - s V_BZ2_hbCreateDecodeTables_i) (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-1 0*) F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                            - s V_BZ2_hbCreateDecodeTables_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))]
     (max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
           - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 42 => (s V_BZ2_hbCreateDecodeTables_z <= z)%Q
   | 43 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (1
                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                              - s V_BZ2_hbCreateDecodeTables_i) (1);
      (*0 0.333333*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*0 0.333333*) F_binom_monotonic 2 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp
                                                         - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0));
      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.333333 6.0418e-12*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.666667*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                         + s V_BZ2_hbCreateDecodeTables__tmp
                                                         - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))]
     (max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
           - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 44 => ((1 # 1)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 45 => ((1 # 1)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 46 => ((1 # 1)
            + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 47 => ((1 # 1)
            + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 48 => ((1 # 1)
            + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))]
     ((1 # 1) + max0(-1 + s V_BZ2_hbCreateDecodeTables_z)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 50 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_BZ2_hbCreateDecodeTables__tmp
                                         - s V_BZ2_hbCreateDecodeTables_i) (1);
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                  - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))]
     (-(1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables_i
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 51 => ((1 # 1)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 52 => ((1 # 1)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 53 => ((1 # 1)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 54 => ((1 # 1)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 55 => ((1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 56 => ((1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 57 => ((1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 58 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))]
     ((1 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
      + max0(-1 + s V_BZ2_hbCreateDecodeTables_z)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 59 => hints
     [(*-0.0455446 0*) F_max0_pre_decrement 1 (23
                                               - s V_BZ2_hbCreateDecodeTables_i) (1);
      (*0 0.0019802*) F_binom_monotonic 2 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0));
      (*0 0.0217822*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0019802 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0198469 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.00198019*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)))]
     ((2566 # 117) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (191 # 100) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                         + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 74) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (23 # 48) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 46) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(22
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
      - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 46) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 101) * max0(23 - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (0 # 1) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 60 => ((967 # 44) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (239 # 125) * s V_BZ2_hbCreateDecodeTables_i
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 47) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 42) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (21 # 46) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 61 => ((967 # 44) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (239 # 125) * s V_BZ2_hbCreateDecodeTables_i
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 47) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (0 # 1) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 42) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (21 # 46) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 62 => ((1053 # 44) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (2 # 1) * s V_BZ2_hbCreateDecodeTables_i
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 47) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
            - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i) * max0(-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (21 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 63 => ((1053 # 44) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (2 # 1) * s V_BZ2_hbCreateDecodeTables_i
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 47) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
            - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i) * max0(-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (21 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 64 => ((1053 # 44) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (2 # 1) * s V_BZ2_hbCreateDecodeTables_i
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                               - s V_BZ2_hbCreateDecodeTables_i)
            + (2 # 47) * s V_BZ2_hbCreateDecodeTables_i^2
            + s V_BZ2_hbCreateDecodeTables_z
            - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
            - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i) * max0(-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (21 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 65 => hints
     [(*-0.0019802 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*0 0.000947052*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0217822 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.000947052 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-4.49194e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.019802 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0018941 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))]
     ((1009 # 44) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (2 # 1) * s V_BZ2_hbCreateDecodeTables_i
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                         + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 46) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 47) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
      - (0 # 1) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i) * max0(-1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (21 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (0 # 1) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 66 => hints
     [(*-0.000188218 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i));
      (*-0.04329 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.0225391*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.00197628 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.00197628 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (22
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.000188218 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.0227273*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.950593*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                         + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))]
     ((9319 # 196) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (178 # 87) * s V_BZ2_hbCreateDecodeTables_i
      - (7 # 162) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                          + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 22) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                         - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 44) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (109 # 112) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      - (7 # 162) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i) * max0(23
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables_i)
      + (2 # 89) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 11) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 67 => ((93 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (25 # 23) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 22) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i)^2 <= z)%Q
   | 68 => ((93 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (25 # 23) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(22
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 22) * max0(22 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * max0(s V_BZ2_hbCreateDecodeTables_i)^2 <= z)%Q
   | 69 => ((2189 # 46) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (25 # 23) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 48) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (8 # 169) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 70 => ((2189 # 46) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (25 # 23) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 48) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (8 # 169) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 71 => ((2189 # 46) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (25 # 23) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                               + s V_BZ2_hbCreateDecodeTables_i)
            + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 48) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (8 # 169) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 72 => hints
     [(*-0.000188218 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                             + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0));
      (*-0.020751 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-2
                                                                    + s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.020751 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.0434783*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.000188218 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (23
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.958498 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_i))]
     ((2143 # 46) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (25 # 23) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 48) * s V_BZ2_hbCreateDecodeTables_i * max0(-2
                                                         + s V_BZ2_hbCreateDecodeTables_i)
      + (0 # 1) * s V_BZ2_hbCreateDecodeTables_i * max0(23
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables_z
      + (1 # 48) * max0(-2 + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 44) * max0(-1 + s V_BZ2_hbCreateDecodeTables_i)^2
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (8 # 169) * max0(23 - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 73 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp1
                                                  - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))]
     ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1
             - s V_BZ2_hbCreateDecodeTables_i) <= z)%Q
   | 74 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp1
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 75 => ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp1
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 76 => ((91 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp1
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 77 => ((91 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp1
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 78 => ((91 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp1
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 79 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                               - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_i))]
     ((89 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp1
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2) <= z)%Q
   | 80 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 81 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 82 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 83 => ((137 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 84 => ((137 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 85 => ((137 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 86 => ((135 # 2) - s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i + s V_BZ2_hbCreateDecodeTables_z
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 87 => hints
     [(*0 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))]
     ((68 # 1)
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i^2
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 88 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              - s V_BZ2_hbCreateDecodeTables__tmp
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 89 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                       - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              - s V_BZ2_hbCreateDecodeTables__tmp
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                    - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                            - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 90 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))]
     ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                 - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                              - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                        - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                      - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
      + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                       - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z)
      + max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 91 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                       - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                              - s V_BZ2_hbCreateDecodeTables__tmp
                                                              + s V_BZ2_hbCreateDecodeTables_i)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                    - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                            - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i)^2
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 92 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                              + s V_BZ2_hbCreateDecodeTables__tmp
                                                              - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                     + s V_BZ2_hbCreateDecodeTables__tmp
                                                     - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))]
     ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                 - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                              - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables_z
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                        - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                      - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
      + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                       - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 93 => ((137 # 2)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 94 => ((137 # 2)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 95 => ((139 # 2)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (2 # 1) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables_j) <= z)%Q
   | 96 => ((139 # 2)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (2 # 1) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables_j) <= z)%Q
   | 97 => ((139 # 2)
            + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables__tmp2)
            + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
            + s V_BZ2_hbCreateDecodeTables__tmp2
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
            - s V_BZ2_hbCreateDecodeTables_i
            - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
            + s V_BZ2_hbCreateDecodeTables_z
            + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                              - s V_BZ2_hbCreateDecodeTables_j)
            - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                             + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp
                   - s V_BZ2_hbCreateDecodeTables__tmp2)
            + (2 # 1) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
            + max0(s V_BZ2_hbCreateDecodeTables__tmp1
                   - s V_BZ2_hbCreateDecodeTables_j)
            + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                             - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z)
            - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables_j) <= z)%Q
   | 98 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                            - s V_BZ2_hbCreateDecodeTables_j) (-1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                            - s V_BZ2_hbCreateDecodeTables_j);
      (*0 0.5*) F_binom_monotonic 2 (F_max0_ge_arg (1
                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp
                                                                    + s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                 + s V_BZ2_hbCreateDecodeTables__tmp
                                                                 - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))]
     ((137 # 2)
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - s V_BZ2_hbCreateDecodeTables_i
      - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
      + s V_BZ2_hbCreateDecodeTables_z
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                        - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_z) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_z) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_j)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (3 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + (3 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                       - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables_j) <= z)%Q
   | 99 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1)) (F_check_ge (0) (0)));
      (*-0.5 -9.30533e-12*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp1
                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))]
     ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                 - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      + s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                              - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables_z
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_z * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                        - s V_BZ2_hbCreateDecodeTables_j)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                      - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1) * max0(s V_BZ2_hbCreateDecodeTables_z)
      + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                       - s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables_z) <= z)%Q
   | 100 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                              - s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 101 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                              - s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 102 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                              - s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 103 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                              - s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 104 => ((68 # 1) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables_j)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables__tmp1
                              - s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables_j)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 105 => ((137 # 2) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  + s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 106 => ((137 # 2) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  + s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 107 => ((137 # 2) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
             + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp
                                                                  + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                                  + s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                                  + s V_BZ2_hbCreateDecodeTables__tmp
                                                                  - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables__tmp2
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               - s V_BZ2_hbCreateDecodeTables__tmp
                                                               + s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                               + s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
             - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                               + s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                               - s V_BZ2_hbCreateDecodeTables_i)
             + s V_BZ2_hbCreateDecodeTables_z
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i)
             - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                              + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)
             + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables__tmp2)
             + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables_i)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)
             + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)^2
             + max0(s V_BZ2_hbCreateDecodeTables__tmp
                    - s V_BZ2_hbCreateDecodeTables__tmp2)
             - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                              - s V_BZ2_hbCreateDecodeTables_i)
             + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | 108 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_BZ2_hbCreateDecodeTables__tmp
                                       - s V_BZ2_hbCreateDecodeTables_i) (1);
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (1
                                                                    + s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp1)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                - s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1
                                                                    - s V_BZ2_hbCreateDecodeTables_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_j) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - s V_BZ2_hbCreateDecodeTables_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables_j) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbCreateDecodeTables_z)) (F_check_ge (s V_BZ2_hbCreateDecodeTables_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbCreateDecodeTables__tmp1) (0))) (F_max0_ge_0 (s V_BZ2_hbCreateDecodeTables__tmp1));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_BZ2_hbCreateDecodeTables_j)) (F_check_ge (-1
                                                                    + s V_BZ2_hbCreateDecodeTables_j) (0))]
     ((135 # 2) - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp
      + s V_BZ2_hbCreateDecodeTables__tmp * s V_BZ2_hbCreateDecodeTables__tmp2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           - s V_BZ2_hbCreateDecodeTables__tmp
                                                           + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(-1
                                                           + s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp * max0(1
                                                           + s V_BZ2_hbCreateDecodeTables__tmp
                                                           - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp^2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp1 * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables__tmp2
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(-1
                                                            - s V_BZ2_hbCreateDecodeTables__tmp
                                                            + s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2 * max0(1
                                                            + s V_BZ2_hbCreateDecodeTables__tmp
                                                            - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables__tmp2^2
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        - s V_BZ2_hbCreateDecodeTables__tmp
                                                        + s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(-1
                                                        + s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_i * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      - s V_BZ2_hbCreateDecodeTables_i * max0(s V_BZ2_hbCreateDecodeTables__tmp1)
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j
      - (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(1
                                                        + s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * s V_BZ2_hbCreateDecodeTables_j * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                        - s V_BZ2_hbCreateDecodeTables_i)
      + s V_BZ2_hbCreateDecodeTables_z
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables__tmp2) * max0(1
                                                                    + 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp
                                                                    - 
                                                                    s V_BZ2_hbCreateDecodeTables__tmp2)
      + (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i)
      - (1 # 2) * max0(-1 - s V_BZ2_hbCreateDecodeTables__tmp
                       + s V_BZ2_hbCreateDecodeTables_i) * max0(1
                                                                + s V_BZ2_hbCreateDecodeTables__tmp
                                                                - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(1
                                                                   + 
                                                                   s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * max0(-1 + s V_BZ2_hbCreateDecodeTables_j) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                                                                   - 
                                                                   s V_BZ2_hbCreateDecodeTables_i)
      + (3 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables__tmp2)
      + max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables_i)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i) * max0(s V_BZ2_hbCreateDecodeTables__tmp1
                                                                - s V_BZ2_hbCreateDecodeTables_j)
      + (1 # 2) * max0(1 + s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)^2
      + max0(s V_BZ2_hbCreateDecodeTables__tmp
             - s V_BZ2_hbCreateDecodeTables__tmp2)
      - (1 # 2) * max0(s V_BZ2_hbCreateDecodeTables__tmp
                       - s V_BZ2_hbCreateDecodeTables_i)
      + max0(s V_BZ2_hbCreateDecodeTables__tmp1) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BZ2_hbCreateDecodeTables =>
    [mkPA Q (fun n z s => ai_BZ2_hbCreateDecodeTables n s /\ annot0_BZ2_hbCreateDecodeTables n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BZ2_hbCreateDecodeTables (proc_start P_BZ2_hbCreateDecodeTables) s1 (proc_end P_BZ2_hbCreateDecodeTables) s2 ->
    (s2 V_BZ2_hbCreateDecodeTables_z <= (68 # 1)
                                        + (2 # 1) * max0(1
                                                         + s1 V_BZ2_hbCreateDecodeTables_maxLen
                                                         - s1 V_BZ2_hbCreateDecodeTables_minLen)
                                        + max0(1
                                               + s1 V_BZ2_hbCreateDecodeTables_maxLen
                                               - s1 V_BZ2_hbCreateDecodeTables_minLen) * max0(s1 V_BZ2_hbCreateDecodeTables_alphaSize)
                                        + max0(s1 V_BZ2_hbCreateDecodeTables_alphaSize)
                                        + max0(s1 V_BZ2_hbCreateDecodeTables_maxLen
                                               - s1 V_BZ2_hbCreateDecodeTables_minLen))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BZ2_hbCreateDecodeTables.
Qed.
