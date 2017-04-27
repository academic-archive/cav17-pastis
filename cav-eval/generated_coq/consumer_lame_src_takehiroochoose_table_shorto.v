Require Import pasta.Pasta.

Inductive proc: Type :=
  P_choose_table_short.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_choose_table_short_z := 1%positive.
Notation V_choose_table_short__tmp := 2%positive.
Notation V_choose_table_short_choice0 := 3%positive.
Notation V_choose_table_short_choice1 := 4%positive.
Notation V_choose_table_short_max := 5%positive.
Notation V_choose_table_short_sum0 := 6%positive.
Notation V_choose_table_short_sum1 := 7%positive.
Notation V_choose_table_short_end := 8%positive.
Notation V_choose_table_short_ix := 9%positive.
Notation V_choose_table_short_s := 10%positive.
Definition Pedges_choose_table_short: list (edge proc) :=
  (EA 1 (AAssign V_choose_table_short_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_choose_table_short_max None) 3)::(EA 3 AWeaken 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_max) s) > (eval (ENum (8206))
  s))%Z)) 86)::(EA 4 (AGuard (fun s => ((eval (EVar V_choose_table_short_max)
  s) <= (eval (ENum (8206)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_max) s) <= (eval (ENum (15))
  s))%Z)) 39)::(EA 6 (AGuard (fun s => ((eval (EVar V_choose_table_short_max)
  s) > (eval (ENum (15)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_choose_table_short_max (Some (ESub (EVar V_choose_table_short_max)
  (ENum (15))))) 9)::(EA 9 (AAssign V_choose_table_short_choice1
  (Some (ENum (24)))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard (fun s => ((eval (EVar V_choose_table_short_choice1) s) <
  (eval (ENum (32)) s))%Z)) 14)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_choice1) s) >=
  (eval (ENum (32)) s))%Z)) 13)::(EA 13 AWeaken 23)::(EA 14 AWeaken 15)::
  (EA 15 ANone 22)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_choose_table_short_choice1
  (Some (EAdd (EVar V_choose_table_short_choice1) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_choose_table_short_z
  (Some (EAdd (ENum (1)) (EVar V_choose_table_short_z)))) 21)::
  (EA 21 AWeaken 12)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_choose_table_short_choice0
  (Some (ESub (EVar V_choose_table_short_choice1) (ENum (8))))) 24)::
  (EA 24 ANone 25)::(EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_choice0) s) < (eval (ENum (24))
  s))%Z)) 28)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_choice0) s) >=
  (eval (ENum (24)) s))%Z)) 27)::(EA 27 AWeaken 37)::(EA 28 AWeaken 29)::
  (EA 29 ANone 36)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_choose_table_short_choice0
  (Some (EAdd (EVar V_choose_table_short_choice0) (ENum (1))))) 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign V_choose_table_short_z
  (Some (EAdd (ENum (1)) (EVar V_choose_table_short_z)))) 35)::
  (EA 35 AWeaken 26)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_choose_table_short_choice0 None) 38)::(EA 38 ANone 79)::
  (EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_max) s) = (eval (ENum (0))
  s))%Z)) 82)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_max) s) <> (eval (ENum (0))
  s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 (AAssign
  V_choose_table_short_choice0 None) 43)::(EA 43 (AAssign
  V_choose_table_short_sum0 None) 44)::(EA 44 (AAssign
  V_choose_table_short_choice1
  (Some (EVar V_choose_table_short_choice0))) 45)::(EA 45 AWeaken 46)::
  (EA 46 ANone 77)::(EA 46 ANone 57)::(EA 46 ANone 57)::(EA 46 ANone 67)::
  (EA 46 ANone 67)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_choose_table_short_choice1
  (Some (EAdd (EVar V_choose_table_short_choice1) (ENum (2))))) 48)::
  (EA 48 (AAssign V_choose_table_short_sum1 None) 49)::(EA 49 AWeaken 50)::
  (EA 50 (AGuard (fun s => ((eval (EVar V_choose_table_short_sum0) s) >
  (eval (EVar V_choose_table_short_sum1) s))%Z)) 52)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_sum0) s) <=
  (eval (EVar V_choose_table_short_sum1) s))%Z)) 51)::(EA 51 AWeaken 56)::
  (EA 52 AWeaken 53)::(EA 53 (AAssign V_choose_table_short_sum0
  (Some (EVar V_choose_table_short_sum1))) 54)::(EA 54 (AAssign
  V_choose_table_short_choice0
  (Some (EVar V_choose_table_short_choice1))) 55)::(EA 55 ANone 56)::
  (EA 56 ANone 78)::(EA 57 (AAssign V_choose_table_short_choice1
  (Some (EAdd (EVar V_choose_table_short_choice1) (ENum (1))))) 58)::
  (EA 58 (AAssign V_choose_table_short_sum1 None) 59)::(EA 59 AWeaken 60)::
  (EA 60 (AGuard (fun s => ((eval (EVar V_choose_table_short_sum0) s) >
  (eval (EVar V_choose_table_short_sum1) s))%Z)) 62)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_sum0) s) <=
  (eval (EVar V_choose_table_short_sum1) s))%Z)) 61)::(EA 61 AWeaken 66)::
  (EA 62 AWeaken 63)::(EA 63 (AAssign V_choose_table_short_sum0
  (Some (EVar V_choose_table_short_sum1))) 64)::(EA 64 (AAssign
  V_choose_table_short_choice0
  (Some (EVar V_choose_table_short_choice1))) 65)::(EA 65 ANone 66)::
  (EA 66 ANone 67)::(EA 67 (AAssign V_choose_table_short_choice1
  (Some (EAdd (EVar V_choose_table_short_choice1) (ENum (1))))) 68)::
  (EA 68 (AAssign V_choose_table_short_sum1 None) 69)::(EA 69 AWeaken 70)::
  (EA 70 (AGuard (fun s => ((eval (EVar V_choose_table_short_sum0) s) >
  (eval (EVar V_choose_table_short_sum1) s))%Z)) 72)::(EA 70 (AGuard
  (fun s => ((eval (EVar V_choose_table_short_sum0) s) <=
  (eval (EVar V_choose_table_short_sum1) s))%Z)) 71)::(EA 71 AWeaken 76)::
  (EA 72 AWeaken 73)::(EA 73 (AAssign V_choose_table_short_sum0
  (Some (EVar V_choose_table_short_sum1))) 74)::(EA 74 (AAssign
  V_choose_table_short_choice0
  (Some (EVar V_choose_table_short_choice1))) 75)::(EA 75 ANone 76)::
  (EA 76 ANone 78)::(EA 77 ANone 78)::(EA 78 ANone 79)::(EA 79 (AAssign
  V_choose_table_short__tmp (Some (EVar V_choose_table_short_choice0))) 80)::
  (EA 80 ANone 81)::(EA 81 AWeaken 90)::(EA 82 AWeaken 83)::(EA 83 (AAssign
  V_choose_table_short__tmp (Some (ENum (0)))) 84)::(EA 84 ANone 85)::
  (EA 85 AWeaken 90)::(EA 86 AWeaken 87)::(EA 87 (AAssign
  V_choose_table_short__tmp (Some (ENum (-1)))) 88)::(EA 88 ANone 89)::
  (EA 89 AWeaken 90)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_choose_table_short => Pedges_choose_table_short
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_choose_table_short => 90
     end)%positive;
  var_global := var_global
}.

Definition ai_choose_table_short (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 3 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0)%Z
   | 4 => (1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 5 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -8206 <= 0)%Z
   | 6 => (1 * s V_choose_table_short_max + -8206 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 7 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -8206 <= 0 /\ -1 * s V_choose_table_short_max + 16 <= 0)%Z
   | 8 => (-1 * s V_choose_table_short_max + 16 <= 0 /\ 1 * s V_choose_table_short_max + -8206 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 9 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 10 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -24 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0)%Z
   | 11 => (-1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 12 => (-1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0)%Z
   | 13 => (1 * s V_choose_table_short_choice1 + -32 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice1 + 32 <= 0)%Z
   | 14 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -31 <= 0)%Z
   | 15 => (1 * s V_choose_table_short_choice1 + -31 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 16 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -31 <= 0)%Z
   | 17 => (1 * s V_choose_table_short_choice1 + -31 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 18 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 25 <= 0)%Z
   | 19 => (-1 * s V_choose_table_short_choice1 + 25 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 20 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 25 <= 0)%Z
   | 21 => (-1 * s V_choose_table_short_choice1 + 25 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_z + 1 <= 0)%Z
   | 22 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -31 <= 0)%Z
   | 23 => (1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 24 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0)%Z
   | 25 => (-1 * s V_choose_table_short_choice0 + 16 <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 26 => (-1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0)%Z
   | 27 => (1 * s V_choose_table_short_choice0 + -24 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice0 + 24 <= 0)%Z
   | 28 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice0 + -23 <= 0)%Z
   | 29 => (1 * s V_choose_table_short_choice0 + -23 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 30 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice0 + -23 <= 0)%Z
   | 31 => (1 * s V_choose_table_short_choice0 + -23 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 32 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0 /\ -1 * s V_choose_table_short_choice0 + 17 <= 0)%Z
   | 33 => (-1 * s V_choose_table_short_choice0 + 17 <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 34 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0 /\ -1 * s V_choose_table_short_choice0 + 17 <= 0)%Z
   | 35 => (-1 * s V_choose_table_short_choice0 + 17 <= 0 /\ 1 * s V_choose_table_short_choice0 + -24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_z + 1 <= 0)%Z
   | 36 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_choice0 + -23 <= 0)%Z
   | 37 => (1 * s V_choose_table_short_choice0 + -24 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_choice0 + 16 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 38 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_max + 1 <= 0 /\ -1 * s V_choose_table_short_choice1 + 24 <= 0 /\ 1 * s V_choose_table_short_choice1 + -32 <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 39 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 40 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 41 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 42 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 43 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 44 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 45 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 46 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 47 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 48 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 49 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 50 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 51 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_sum0+ -1 * s V_choose_table_short_sum1 <= 0)%Z
   | 52 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0 /\ -1 * s V_choose_table_short_sum0+ 1 * s V_choose_table_short_sum1 + 1 <= 0)%Z
   | 53 => (-1 * s V_choose_table_short_sum0+ 1 * s V_choose_table_short_sum1 + 1 <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 54 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 55 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 56 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 57 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 58 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 59 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 60 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 61 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_sum0+ -1 * s V_choose_table_short_sum1 <= 0)%Z
   | 62 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0 /\ -1 * s V_choose_table_short_sum0+ 1 * s V_choose_table_short_sum1 + 1 <= 0)%Z
   | 63 => (-1 * s V_choose_table_short_sum0+ 1 * s V_choose_table_short_sum1 + 1 <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 64 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 65 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 66 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 67 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 68 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 69 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 70 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 71 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_sum0+ -1 * s V_choose_table_short_sum1 <= 0)%Z
   | 72 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_sum0+ 1 * s V_choose_table_short_sum1 + 1 <= 0)%Z
   | 73 => (-1 * s V_choose_table_short_sum0+ 1 * s V_choose_table_short_sum1 + 1 <= 0 /\ -1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 74 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 75 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 76 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 77 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -15 <= 0)%Z
   | 78 => (1 * s V_choose_table_short_max + -15 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 79 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 80 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max + -8191 <= 0)%Z
   | 81 => (1 * s V_choose_table_short_max + -8191 <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 82 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max <= 0 /\ -1 * s V_choose_table_short_max <= 0)%Z
   | 83 => (-1 * s V_choose_table_short_max <= 0 /\ 1 * s V_choose_table_short_max <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 84 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_max <= 0 /\ -1 * s V_choose_table_short_max <= 0 /\ 1 * s V_choose_table_short__tmp <= 0 /\ -1 * s V_choose_table_short__tmp <= 0)%Z
   | 85 => (-1 * s V_choose_table_short__tmp <= 0 /\ 1 * s V_choose_table_short__tmp <= 0 /\ -1 * s V_choose_table_short_max <= 0 /\ 1 * s V_choose_table_short_max <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 86 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_max + 8207 <= 0)%Z
   | 87 => (-1 * s V_choose_table_short_max + 8207 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 88 => (-1 * s V_choose_table_short_z <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_max + 8207 <= 0 /\ 1 * s V_choose_table_short__tmp + 1 <= 0 /\ -1 * s V_choose_table_short__tmp + -1 <= 0)%Z
   | 89 => (-1 * s V_choose_table_short__tmp + -1 <= 0 /\ 1 * s V_choose_table_short__tmp + 1 <= 0 /\ -1 * s V_choose_table_short_max + 8207 <= 0 /\ 1 * s V_choose_table_short_z <= 0 /\ -1 * s V_choose_table_short_z <= 0)%Z
   | 90 => (-1 * s V_choose_table_short_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_choose_table_short (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (-(0 # 1) - (0 # 1) * max0(-32 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
           - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 2 => (-(0 # 1) - (0 # 1) * max0(-32 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
           - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 3 => (-(0 # 1) - (0 # 1) * max0(-32 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
           - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 4 => (-(0 # 1) - (0 # 1) * max0(-32 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
           - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 5 => (-(0 # 1) - (0 # 1) * max0(-32 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
           - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
           - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 6 => (-(0 # 1) <= z)%Q
   | 7 => (-(0 # 1) <= z)%Q
   | 8 => (-(0 # 1) <= z)%Q
   | 9 => (-(0 # 1) <= z)%Q
   | 10 => (-(0 # 1) <= z)%Q
   | 11 => hints
     [(*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_choose_table_short_z) (0))) (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_choose_table_short_z) (0))) (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)))]
     (-(0 # 1) <= z)%Q
   | 12 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 13 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 14 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 15 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 16 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 17 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 18 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 19 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 20 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 21 => hints
     [(*0 0.000121862*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_choose_table_short_z)) (F_check_ge (-1
                                                                    + s V_choose_table_short_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)))]
     (-(0 # 1)
      + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                  - s V_choose_table_short_max)
      + (0 # 1) * max0(-1 + s V_choose_table_short_z) * max0(s V_choose_table_short_max)
      - (0 # 1) * max0(8206 - s V_choose_table_short_max) <= z)%Q
   | 22 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 23 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 24 => (-(0 # 1)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 25 => hints
     [(*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_choose_table_short_max)) (F_check_ge (s V_choose_table_short_max) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_choose_table_short_z)) (F_check_ge (s V_choose_table_short_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)))]
     (-(0 # 1)
      + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                  - s V_choose_table_short_max)
      + (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 26 => ((0 # 1) * s V_choose_table_short_max * max0(s V_choose_table_short_z)
            + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                        - s V_choose_table_short_max)
            + (0 # 1) * s V_choose_table_short_z * max0(s V_choose_table_short_max)
            - (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 27 => hints
     [(*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (8206
                                                                    - s V_choose_table_short_max) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_choose_table_short_z)) (F_check_ge (s V_choose_table_short_z) (0))]
     ((0 # 1) * s V_choose_table_short_max * max0(s V_choose_table_short_z)
      + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                  - s V_choose_table_short_max)
      + (0 # 1) * s V_choose_table_short_z * max0(s V_choose_table_short_max)
      - (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 28 => hints
     [(*0 0.000121862*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (8206
                                                                    - s V_choose_table_short_max) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*0 0.000121862*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*0 0.000121862*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_choose_table_short_z)) (F_check_ge (s V_choose_table_short_z) (0))]
     ((0 # 1) * s V_choose_table_short_max * max0(s V_choose_table_short_z)
      + (0 # 1) * s V_choose_table_short_z * max0(8206
                                                  - s V_choose_table_short_max)
      + (0 # 1) * s V_choose_table_short_z * max0(s V_choose_table_short_max)
      - (0 # 1) * max0(s V_choose_table_short_max) * max0(s V_choose_table_short_z) <= z)%Q
   | 29 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 30 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 31 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 32 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 33 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 34 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 35 => hints
     [(*0 0.000121862*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_choose_table_short_z)) (F_check_ge (-1
                                                                    + s V_choose_table_short_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (8206
                                                                    - s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*0 0.000121862*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_choose_table_short_z)) (F_check_ge (-1
                                                                    + s V_choose_table_short_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8206
                                                                    - s V_choose_table_short_max) (0))) (F_max0_ge_0 (8206
                                                                    - s V_choose_table_short_max))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_max) (0))) (F_max0_ge_0 (s V_choose_table_short_max))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.000121862 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_choose_table_short_max)) (F_check_ge (s V_choose_table_short_max) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_choose_table_short_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_choose_table_short_z))]
     (-(1 # 1) + s V_choose_table_short_z
      + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 36 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 37 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 38 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 39 => (-(0 # 1) <= z)%Q
   | 40 => (-(0 # 1) <= z)%Q
   | 41 => (-(0 # 1) <= z)%Q
   | 42 => (-(0 # 1) <= z)%Q
   | 43 => (-(0 # 1) <= z)%Q
   | 44 => (-(0 # 1) <= z)%Q
   | 45 => hints
     [(*4.75808e-07 0*) F_one;
      (*-1 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_choose_table_short_z) (0))) (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_choose_table_short_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_choose_table_short_z)) (F_check_ge (s V_choose_table_short_z) (0))]
     (-(0 # 1) <= z)%Q
   | 46 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 47 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 48 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 49 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 50 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 51 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 52 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 53 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 54 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 55 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 56 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 57 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 58 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 59 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 60 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 61 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 62 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 63 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 64 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 65 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 66 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 67 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 68 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 69 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 70 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 71 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 72 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 73 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 74 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 75 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 76 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 77 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 78 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 79 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 80 => (s V_choose_table_short_z
            + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 81 => hints
     [(*-0.000121862 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0))]
     (s V_choose_table_short_z + (0 # 1) * max0(s V_choose_table_short_max) <= z)%Q
   | 82 => (-(0 # 1) <= z)%Q
   | 83 => (-(0 # 1) <= z)%Q
   | 84 => (-(0 # 1)
            + (0 # 1) * max0(1 + s V_choose_table_short__tmp) * max0(s V_choose_table_short_max)
            + (1 # 2) * max0(1 + s V_choose_table_short__tmp) * max0(-
                                                                    s V_choose_table_short_z)
            - (0 # 1) * max0(s V_choose_table_short_max)
            - (1 # 2) * max0(-s V_choose_table_short_z) <= z)%Q
   | 85 => hints
     [(*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0));
      (*-2.23163e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_choose_table_short__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_max)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                              + s V_choose_table_short__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_choose_table_short_z) (0))) (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_choose_table_short_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_choose_table_short_z)) (F_check_ge (-
                                                                    s V_choose_table_short_z) (0))]
     (-(0 # 1)
      + (0 # 1) * max0(1 + s V_choose_table_short__tmp) * max0(s V_choose_table_short_max)
      + (1 # 2) * max0(1 + s V_choose_table_short__tmp) * max0(-s V_choose_table_short_z)
      - (0 # 1) * max0(s V_choose_table_short_max)
      - (1 # 2) * max0(-s V_choose_table_short_z) <= z)%Q
   | 86 => hints
     [(*0 -4.75808e-07*) F_one]
     (-(0 # 1) - (0 # 1) * max0(-32 + s V_choose_table_short_choice1)
      - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
      - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
      - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
      - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
      - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
      - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 87 => (-(0 # 1) * max0(-32 + s V_choose_table_short_choice1)
            - (0 # 1) * max0(-25 + s V_choose_table_short_choice1)
            - (0 # 1) * max0(-24 + s V_choose_table_short_choice1)
            - (0 # 1) * max0(-17 + s V_choose_table_short_choice1)
            - (0 # 1) * max0(-16 + s V_choose_table_short_choice1)
            - (0 # 1) * max0(23 - s V_choose_table_short_choice1)
            - (0 # 1) * max0(24 - s V_choose_table_short_choice1) <= z)%Q
   | 88 => (-(0 # 1) * max0(-32 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
            - (0 # 1) * max0(-25 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
            - (0 # 1) * max0(-24 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
            - (0 # 1) * max0(-17 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
            - (0 # 1) * max0(-16 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
            - (0 # 1) * max0(23 - s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
            - (0 # 1) * max0(24 - s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp) <= z)%Q
   | 89 => hints
     [(*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_choose_table_short_z)) (F_check_ge (-
                                                                    s V_choose_table_short_z) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-32
                                                                    + 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-25
                                                                    + 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-24
                                                                    + 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-17
                                                                    + 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                                    + 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (23
                                                                    - 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*1.54356e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (24
                                                                    - 
                                                                    s V_choose_table_short_choice1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_choose_table_short_z) (0))) (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_choose_table_short_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_choose_table_short_z) (0))) (F_max0_ge_0 (s V_choose_table_short_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_choose_table_short_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_choose_table_short_z)) (F_check_ge (0) (0))]
     (-(0 # 1) * max0(-32 + s V_choose_table_short_choice1) * max0(-s V_choose_table_short__tmp)
      - (0 # 1) * max0(-25 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
      - (0 # 1) * max0(-24 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
      - (0 # 1) * max0(-17 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
      - (0 # 1) * max0(-16 + s V_choose_table_short_choice1) * max0(-
                                                                    s V_choose_table_short__tmp)
      - (0 # 1) * max0(23 - s V_choose_table_short_choice1) * max0(-s V_choose_table_short__tmp)
      - (0 # 1) * max0(24 - s V_choose_table_short_choice1) * max0(-s V_choose_table_short__tmp) <= z)%Q
   | 90 => (s V_choose_table_short_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_choose_table_short =>
    [mkPA Q (fun n z s => ai_choose_table_short n s /\ annot0_choose_table_short n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_choose_table_short (proc_start P_choose_table_short) s1 (proc_end P_choose_table_short) s2 ->
    (s2 V_choose_table_short_z <= -(0 # 1)
                                  - (0 # 1) * max0(-32
                                                   + s1 V_choose_table_short_choice1)
                                  - (0 # 1) * max0(-25
                                                   + s1 V_choose_table_short_choice1)
                                  - (0 # 1) * max0(-24
                                                   + s1 V_choose_table_short_choice1)
                                  - (0 # 1) * max0(-17
                                                   + s1 V_choose_table_short_choice1)
                                  - (0 # 1) * max0(-16
                                                   + s1 V_choose_table_short_choice1)
                                  - (0 # 1) * max0(23
                                                   - s1 V_choose_table_short_choice1)
                                  - (0 # 1) * max0(24
                                                   - s1 V_choose_table_short_choice1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_choose_table_short.
Qed.
