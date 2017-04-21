Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_imdct_l.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_imdct_l_z := 1%positive.
Notation V_III_imdct_l__tmp := 2%positive.
Notation V_III_imdct_l_i := 3%positive.
Notation V_III_imdct_l_X := 4%positive.
Notation V_III_imdct_l_block_type := 5%positive.
Notation V_III_imdct_l_z := 6%positive.
Definition Pedges_III_imdct_l: list (edge proc) :=
  (EA 1 (AAssign V_III_imdct_l_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_III_imdct_l__tmp
  (Some (EVar V_III_imdct_l_block_type))) 5)::(EA 5 AWeaken 6)::
  (EA 6 ANone 97)::(EA 6 ANone 83)::(EA 6 ANone 45)::(EA 6 ANone 7)::
  (EA 7 (AAssign V_III_imdct_l_i (Some (ENum (0)))) 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i)
  s) < (eval (ENum (6)) s))%Z)) 38)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (6))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign V_III_imdct_l_i
  (Some (ENum (6)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i) s) <
  (eval (ENum (12)) s))%Z)) 31)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (12))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign V_III_imdct_l_i
  (Some (ENum (18)))) 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i) s) <
  (eval (ENum (36)) s))%Z)) 24)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (36))
  s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 ANone 23)::(EA 23 AWeaken 98)::
  (EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (1))))) 27)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 30)::(EA 30 AWeaken 20)::(EA 31 AWeaken 32)::
  (EA 32 ANone 33)::(EA 33 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (1))))) 34)::(EA 34 ANone 35)::
  (EA 35 ANone 36)::(EA 36 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 37)::(EA 37 AWeaken 15)::(EA 38 AWeaken 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (1))))) 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 44)::(EA 44 AWeaken 10)::(EA 45 (AAssign
  V_III_imdct_l_i (Some (ENum (0)))) 46)::(EA 46 ANone 47)::
  (EA 47 AWeaken 48)::(EA 48 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i)
  s) < (eval (ENum (18)) s))%Z)) 76)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (18))
  s))%Z)) 49)::(EA 49 AWeaken 50)::(EA 50 (AAssign V_III_imdct_l_i
  (Some (ENum (24)))) 51)::(EA 51 ANone 52)::(EA 52 AWeaken 53)::
  (EA 53 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i) s) <
  (eval (ENum (30)) s))%Z)) 69)::(EA 53 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (30))
  s))%Z)) 54)::(EA 54 AWeaken 55)::(EA 55 (AAssign V_III_imdct_l_i
  (Some (ENum (30)))) 56)::(EA 56 ANone 57)::(EA 57 AWeaken 58)::
  (EA 58 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i) s) <
  (eval (ENum (36)) s))%Z)) 62)::(EA 58 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (36))
  s))%Z)) 59)::(EA 59 AWeaken 60)::(EA 60 ANone 61)::(EA 61 AWeaken 98)::
  (EA 62 AWeaken 63)::(EA 63 ANone 64)::(EA 64 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (1))))) 65)::(EA 65 ANone 66)::
  (EA 66 ANone 67)::(EA 67 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 68)::(EA 68 AWeaken 58)::(EA 69 AWeaken 70)::
  (EA 70 ANone 71)::(EA 71 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (1))))) 72)::(EA 72 ANone 73)::
  (EA 73 ANone 74)::(EA 74 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 75)::(EA 75 AWeaken 53)::(EA 76 AWeaken 77)::
  (EA 77 ANone 78)::(EA 78 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (1))))) 79)::(EA 79 ANone 80)::
  (EA 80 ANone 81)::(EA 81 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 82)::(EA 82 AWeaken 48)::(EA 83 (AAssign
  V_III_imdct_l_i (Some (ENum (0)))) 84)::(EA 84 ANone 85)::
  (EA 85 AWeaken 86)::(EA 86 (AGuard (fun s => ((eval (EVar V_III_imdct_l_i)
  s) < (eval (ENum (36)) s))%Z)) 90)::(EA 86 (AGuard
  (fun s => ((eval (EVar V_III_imdct_l_i) s) >= (eval (ENum (36))
  s))%Z)) 87)::(EA 87 AWeaken 88)::(EA 88 ANone 89)::(EA 89 AWeaken 98)::
  (EA 90 AWeaken 91)::(EA 91 ANone 92)::(EA 92 (AAssign V_III_imdct_l_i
  (Some (EAdd (EVar V_III_imdct_l_i) (ENum (4))))) 93)::(EA 93 ANone 94)::
  (EA 94 ANone 95)::(EA 95 (AAssign V_III_imdct_l_z (Some (EAdd (ENum (1))
  (EVar V_III_imdct_l_z)))) 96)::(EA 96 AWeaken 86)::(EA 97 AWeaken 98)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_imdct_l => Pedges_III_imdct_l
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_imdct_l => 98
     end)%positive;
  var_global := var_global
}.

Definition ai_III_imdct_l (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 3 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 4 => (-1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 5 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 6 => (-1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 7 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 8 => (1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 9 => (-1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0)%Z
   | 10 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_i + -6 <= 0)%Z
   | 11 => (1 * s V_III_imdct_l_i + -6 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 6 <= 0)%Z
   | 12 => (-1 * s V_III_imdct_l_i + 6 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -6 <= 0)%Z
   | 13 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -6 <= 0 /\ -1 * s V_III_imdct_l_i + 6 <= 0)%Z
   | 14 => (-1 * s V_III_imdct_l_i + 6 <= 0 /\ 1 * s V_III_imdct_l_i + -6 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 15 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 6 <= 0 /\ 1 * s V_III_imdct_l_i + -12 <= 0)%Z
   | 16 => (1 * s V_III_imdct_l_i + -12 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 12 <= 0)%Z
   | 17 => (-1 * s V_III_imdct_l_i + 12 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -12 <= 0)%Z
   | 18 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -18 <= 0 /\ -1 * s V_III_imdct_l_i + 18 <= 0)%Z
   | 19 => (-1 * s V_III_imdct_l_i + 18 <= 0 /\ 1 * s V_III_imdct_l_i + -18 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 20 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 18 <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 21 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 36 <= 0)%Z
   | 22 => (-1 * s V_III_imdct_l_i + 36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 23 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 36 <= 0)%Z
   | 24 => (-1 * s V_III_imdct_l_i + 18 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -35 <= 0)%Z
   | 25 => (1 * s V_III_imdct_l_i + -35 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 18 <= 0)%Z
   | 26 => (-1 * s V_III_imdct_l_i + 18 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -35 <= 0)%Z
   | 27 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 19 <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 28 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_i + 19 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 29 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 19 <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 30 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_i + 19 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 31 => (-1 * s V_III_imdct_l_i + 6 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -11 <= 0)%Z
   | 32 => (1 * s V_III_imdct_l_i + -11 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 6 <= 0)%Z
   | 33 => (-1 * s V_III_imdct_l_i + 6 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -11 <= 0)%Z
   | 34 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 7 <= 0 /\ 1 * s V_III_imdct_l_i + -12 <= 0)%Z
   | 35 => (1 * s V_III_imdct_l_i + -12 <= 0 /\ -1 * s V_III_imdct_l_i + 7 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 36 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 7 <= 0 /\ 1 * s V_III_imdct_l_i + -12 <= 0)%Z
   | 37 => (1 * s V_III_imdct_l_i + -12 <= 0 /\ -1 * s V_III_imdct_l_i + 7 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 38 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -5 <= 0)%Z
   | 39 => (1 * s V_III_imdct_l_i + -5 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 40 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -5 <= 0)%Z
   | 41 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ 1 * s V_III_imdct_l_i + -6 <= 0)%Z
   | 42 => (1 * s V_III_imdct_l_i + -6 <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 43 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ 1 * s V_III_imdct_l_i + -6 <= 0)%Z
   | 44 => (1 * s V_III_imdct_l_i + -6 <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 45 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 46 => (1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 47 => (-1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0)%Z
   | 48 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_i + -18 <= 0)%Z
   | 49 => (1 * s V_III_imdct_l_i + -18 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 18 <= 0)%Z
   | 50 => (-1 * s V_III_imdct_l_i + 18 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -18 <= 0)%Z
   | 51 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -24 <= 0 /\ -1 * s V_III_imdct_l_i + 24 <= 0)%Z
   | 52 => (-1 * s V_III_imdct_l_i + 24 <= 0 /\ 1 * s V_III_imdct_l_i + -24 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 53 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 24 <= 0 /\ 1 * s V_III_imdct_l_i + -30 <= 0)%Z
   | 54 => (1 * s V_III_imdct_l_i + -30 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 30 <= 0)%Z
   | 55 => (-1 * s V_III_imdct_l_i + 30 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -30 <= 0)%Z
   | 56 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -30 <= 0 /\ -1 * s V_III_imdct_l_i + 30 <= 0)%Z
   | 57 => (-1 * s V_III_imdct_l_i + 30 <= 0 /\ 1 * s V_III_imdct_l_i + -30 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 58 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 30 <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 59 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 36 <= 0)%Z
   | 60 => (-1 * s V_III_imdct_l_i + 36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 61 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 36 <= 0)%Z
   | 62 => (-1 * s V_III_imdct_l_i + 30 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -35 <= 0)%Z
   | 63 => (1 * s V_III_imdct_l_i + -35 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 30 <= 0)%Z
   | 64 => (-1 * s V_III_imdct_l_i + 30 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -35 <= 0)%Z
   | 65 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 31 <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 66 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_i + 31 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 67 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 31 <= 0 /\ 1 * s V_III_imdct_l_i + -36 <= 0)%Z
   | 68 => (1 * s V_III_imdct_l_i + -36 <= 0 /\ -1 * s V_III_imdct_l_i + 31 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 69 => (-1 * s V_III_imdct_l_i + 24 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -29 <= 0)%Z
   | 70 => (1 * s V_III_imdct_l_i + -29 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 24 <= 0)%Z
   | 71 => (-1 * s V_III_imdct_l_i + 24 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -29 <= 0)%Z
   | 72 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 25 <= 0 /\ 1 * s V_III_imdct_l_i + -30 <= 0)%Z
   | 73 => (1 * s V_III_imdct_l_i + -30 <= 0 /\ -1 * s V_III_imdct_l_i + 25 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 74 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 25 <= 0 /\ 1 * s V_III_imdct_l_i + -30 <= 0)%Z
   | 75 => (1 * s V_III_imdct_l_i + -30 <= 0 /\ -1 * s V_III_imdct_l_i + 25 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 76 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -17 <= 0)%Z
   | 77 => (1 * s V_III_imdct_l_i + -17 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 78 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -17 <= 0)%Z
   | 79 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ 1 * s V_III_imdct_l_i + -18 <= 0)%Z
   | 80 => (1 * s V_III_imdct_l_i + -18 <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 81 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ 1 * s V_III_imdct_l_i + -18 <= 0)%Z
   | 82 => (1 * s V_III_imdct_l_i + -18 <= 0 /\ -1 * s V_III_imdct_l_i + 1 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 83 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 84 => (1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 85 => (-1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0)%Z
   | 86 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0 /\ 1 * s V_III_imdct_l_i + -39 <= 0)%Z
   | 87 => (1 * s V_III_imdct_l_i + -39 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 36 <= 0)%Z
   | 88 => (-1 * s V_III_imdct_l_i + 36 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -39 <= 0)%Z
   | 89 => (1 * s V_III_imdct_l_i + -39 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 36 <= 0)%Z
   | 90 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -35 <= 0)%Z
   | 91 => (1 * s V_III_imdct_l_i + -35 <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 92 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_i + -35 <= 0)%Z
   | 93 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 4 <= 0 /\ 1 * s V_III_imdct_l_i + -39 <= 0)%Z
   | 94 => (1 * s V_III_imdct_l_i + -39 <= 0 /\ -1 * s V_III_imdct_l_i + 4 <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | 95 => (-1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i + 4 <= 0 /\ 1 * s V_III_imdct_l_i + -39 <= 0)%Z
   | 96 => (1 * s V_III_imdct_l_i + -39 <= 0 /\ -1 * s V_III_imdct_l_i + 4 <= 0 /\ -1 * s V_III_imdct_l_z + 1 <= 0)%Z
   | 97 => (-1 * s V_III_imdct_l_z <= 0 /\ 1 * s V_III_imdct_l_z <= 0 /\ -1 * s V_III_imdct_l_i <= 0)%Z
   | 98 => (-1 * s V_III_imdct_l_i <= 0 /\ -1 * s V_III_imdct_l_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_imdct_l (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((30 # 1) <= z)%Q
   | 2 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 3 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 4 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 5 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 6 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 7 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 8 => ((24 # 1) + max0(6 - s V_III_imdct_l_i) + max0(s V_III_imdct_l_z) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_imdct_l_z)) (F_check_ge (s V_III_imdct_l_z) (0))]
     ((24 # 1) + max0(6 - s V_III_imdct_l_i) + max0(s V_III_imdct_l_z) <= z)%Q
   | 10 => ((24 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (6 - s V_III_imdct_l_i) (5
                                                                    - 
                                                                    s V_III_imdct_l_i));
      (*-1 0*) F_max0_ge_0 (5 - s V_III_imdct_l_i)]
     ((24 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 12 => ((24 # 1) + s V_III_imdct_l_z <= z)%Q
   | 13 => (s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 14 => (s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 15 => (s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (12 - s V_III_imdct_l_i) (11
                                                                    - s V_III_imdct_l_i));
      (*-1 0*) F_max0_ge_0 (11 - s V_III_imdct_l_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (18 - s V_III_imdct_l_i)) (F_check_ge (18
                                                                    - s V_III_imdct_l_i) (0))]
     (s V_III_imdct_l_i + s V_III_imdct_l_z + max0(12 - s V_III_imdct_l_i)
      + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 17 => ((18 # 1) + s V_III_imdct_l_z <= z)%Q
   | 18 => (-(4 # 1) + s V_III_imdct_l_z - max0(35 - s V_III_imdct_l_i)
            + max0(36 - s V_III_imdct_l_i) + max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (39 - s V_III_imdct_l_i) (4)]
     (-(4 # 1) + s V_III_imdct_l_z - max0(35 - s V_III_imdct_l_i)
      + max0(36 - s V_III_imdct_l_i) + max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 20 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 21 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 22 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (36 - s V_III_imdct_l_i) (35
                                                                    - s V_III_imdct_l_i));
      (*-1 0*) F_max0_ge_0 (35 - s V_III_imdct_l_i)]
     (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (36 - s V_III_imdct_l_i) (1)]
     (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 25 => ((1 # 1) + s V_III_imdct_l_z + max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 26 => ((1 # 1) + s V_III_imdct_l_z + max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 27 => ((1 # 1) + s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 28 => ((1 # 1) + s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 29 => ((1 # 1) + s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 30 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (12 - s V_III_imdct_l_i) (1);
      (*-1 0*) F_max0_pre_decrement 1 (18 - s V_III_imdct_l_i) (1)]
     (s V_III_imdct_l_i + s V_III_imdct_l_z + max0(12 - s V_III_imdct_l_i)
      + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 32 => ((2 # 1) + s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(11 - s V_III_imdct_l_i) + max0(17 - s V_III_imdct_l_i) <= z)%Q
   | 33 => ((2 # 1) + s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(11 - s V_III_imdct_l_i) + max0(17 - s V_III_imdct_l_i) <= z)%Q
   | 34 => ((1 # 1) + s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 35 => ((1 # 1) + s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 36 => ((1 # 1) + s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 37 => (s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(12 - s V_III_imdct_l_i) + max0(18 - s V_III_imdct_l_i) <= z)%Q
   | 38 => hints
     [(*0 1*) F_max0_pre_decrement 1 (6 - s V_III_imdct_l_i) (1)]
     ((24 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 39 => ((25 # 1) + s V_III_imdct_l_z + max0(5 - s V_III_imdct_l_i) <= z)%Q
   | 40 => ((25 # 1) + s V_III_imdct_l_z + max0(5 - s V_III_imdct_l_i) <= z)%Q
   | 41 => ((25 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 42 => ((25 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 43 => ((25 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 44 => ((24 # 1) + s V_III_imdct_l_z + max0(6 - s V_III_imdct_l_i) <= z)%Q
   | 45 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 46 => ((30 # 1) - s V_III_imdct_l_i + max0(s V_III_imdct_l_z) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_imdct_l_z)) (F_check_ge (s V_III_imdct_l_z) (0))]
     ((30 # 1) - s V_III_imdct_l_i + max0(s V_III_imdct_l_z) <= z)%Q
   | 48 => ((30 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 49 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (18 - s V_III_imdct_l_i) (17
                                                                    - s V_III_imdct_l_i));
      (*-1 0*) F_max0_ge_0 (17 - s V_III_imdct_l_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18
                                                               - s V_III_imdct_l_i) (0))) (F_max0_ge_0 (18
                                                                    - s V_III_imdct_l_i))]
     ((30 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 50 => ((12 # 1) + s V_III_imdct_l_z <= z)%Q
   | 51 => ((6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(30 - s V_III_imdct_l_i)
            + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 52 => ((6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(30 - s V_III_imdct_l_i)
            + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 53 => ((6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(30 - s V_III_imdct_l_i)
            + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 54 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (30 - s V_III_imdct_l_i) (29
                                                                    - s V_III_imdct_l_i));
      (*-1 0*) F_max0_ge_0 (29 - s V_III_imdct_l_i);
      (*-0.171429 0*) F_binom_monotonic 1 (F_max0_ge_arg (35
                                                          - s V_III_imdct_l_i)) (F_check_ge (35
                                                                    - s V_III_imdct_l_i) (0))]
     ((6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
      + max0(30 - s V_III_imdct_l_i)
      + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 55 => ((6 # 1) + s V_III_imdct_l_z <= z)%Q
   | 56 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 57 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 58 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 59 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 60 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 61 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (36 - s V_III_imdct_l_i) (35
                                                                    - s V_III_imdct_l_i));
      (*-1 0*) F_max0_ge_0 (35 - s V_III_imdct_l_i)]
     (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 62 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (36 - s V_III_imdct_l_i) (1)]
     (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 63 => ((1 # 1) + s V_III_imdct_l_z + max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 64 => ((1 # 1) + s V_III_imdct_l_z + max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 65 => ((1 # 1) + s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 66 => ((1 # 1) + s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 67 => ((1 # 1) + s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 68 => (s V_III_imdct_l_z + max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 69 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (30 - s V_III_imdct_l_i) (1)]
     ((6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
      + max0(30 - s V_III_imdct_l_i)
      + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 70 => ((1 # 1) + (6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(29 - s V_III_imdct_l_i)
            + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 71 => ((1 # 1) + (6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(29 - s V_III_imdct_l_i)
            + (6 # 35) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 72 => ((29 # 35) + (6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(30 - s V_III_imdct_l_i)
            + (6 # 35) * max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 73 => ((29 # 35) + (6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(30 - s V_III_imdct_l_i)
            + (6 # 35) * max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 74 => ((29 # 35) + (6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
            + max0(30 - s V_III_imdct_l_i)
            + (6 # 35) * max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 75 => hints
     [(*-0.171429 0*) F_max0_pre_decrement 1 (36 - s V_III_imdct_l_i) (1)]
     (-(6 # 35) + (6 # 35) * s V_III_imdct_l_i + s V_III_imdct_l_z
      + max0(30 - s V_III_imdct_l_i)
      + (6 # 35) * max0(36 - s V_III_imdct_l_i) <= z)%Q
   | 76 => ((30 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 77 => ((30 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 78 => ((30 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 79 => ((31 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 80 => ((31 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 81 => ((31 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 82 => ((30 # 1) - s V_III_imdct_l_i + s V_III_imdct_l_z <= z)%Q
   | 83 => ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 84 => ((9 # 8) * max0(18 - s V_III_imdct_l_i)
            + (1 # 4) * max0(39 - s V_III_imdct_l_i)
            + max0(s V_III_imdct_l_z) <= z)%Q
   | 85 => hints
     [(*-1.125 0*) F_max0_monotonic (F_check_ge (18 - s V_III_imdct_l_i) (17
                                                                    - s V_III_imdct_l_i));
      (*-1.125 0*) F_max0_ge_0 (17 - s V_III_imdct_l_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_imdct_l_z)) (F_check_ge (s V_III_imdct_l_z) (0))]
     ((9 # 8) * max0(18 - s V_III_imdct_l_i)
      + (1 # 4) * max0(39 - s V_III_imdct_l_i) + max0(s V_III_imdct_l_z) <= z)%Q
   | 86 => (s V_III_imdct_l_z + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 87 => (s V_III_imdct_l_z + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 88 => (s V_III_imdct_l_z + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 89 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (39 - s V_III_imdct_l_i) (35
                                                                    - s V_III_imdct_l_i));
      (*-0.25 0*) F_max0_ge_0 (35 - s V_III_imdct_l_i)]
     (s V_III_imdct_l_z + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 90 => hints
     [(*0 0.25*) F_max0_pre_decrement 1 (39 - s V_III_imdct_l_i) (4)]
     (s V_III_imdct_l_z + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 91 => ((1 # 1) + s V_III_imdct_l_z
            + (1 # 4) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 92 => ((1 # 1) + s V_III_imdct_l_z
            + (1 # 4) * max0(35 - s V_III_imdct_l_i) <= z)%Q
   | 93 => ((1 # 1) + s V_III_imdct_l_z
            + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 94 => ((1 # 1) + s V_III_imdct_l_z
            + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 95 => ((1 # 1) + s V_III_imdct_l_z
            + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 96 => (s V_III_imdct_l_z + (1 # 4) * max0(39 - s V_III_imdct_l_i) <= z)%Q
   | 97 => hints
     [(*-30 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_III_imdct_l_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_III_imdct_l_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_III_imdct_l_z) (0))) (F_max0_ge_0 (-
                                                                    s V_III_imdct_l_z))]
     ((30 # 1) + max0(s V_III_imdct_l_z) <= z)%Q
   | 98 => (s V_III_imdct_l_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_imdct_l =>
    [mkPA Q (fun n z s => ai_III_imdct_l n s /\ annot0_III_imdct_l n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_imdct_l (proc_start P_III_imdct_l) s1 (proc_end P_III_imdct_l) s2 ->
    (s2 V_III_imdct_l_z <= (30 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_imdct_l.
Qed.
