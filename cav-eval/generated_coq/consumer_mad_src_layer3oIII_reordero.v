Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_reorder.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_reorder_z := 1%positive.
Notation V_III_reorder_f := 2%positive.
Notation V_III_reorder_l := 3%positive.
Notation V_III_reorder_sb := 4%positive.
Notation V_III_reorder_w := 5%positive.
Notation V_III_reorder_channel := 6%positive.
Notation V_III_reorder_sfbwidth := 7%positive.
Notation V_III_reorder_xr := 8%positive.
Definition Pedges_III_reorder: list (edge proc) :=
  (EA 1 (AAssign V_III_reorder_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_III_reorder_w) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_III_reorder_l) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_III_reorder_sb (Some (ENum (0)))) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::
  (EA 7 ANone 15)::(EA 8 (AAssign V_III_reorder_sb (Some (ENum (2)))) 9)::
  (EA 9 (AAssign V_III_reorder_l (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_III_reorder_l)
  s) < (eval (ENum (36)) s))%Z)) 53)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_III_reorder_l) s) >= (eval (ENum (36))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_III_reorder_w (Some (ENum (0)))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard (fun s => ((eval (EVar V_III_reorder_w)
  s) < (eval (ENum (3)) s))%Z)) 46)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_III_reorder_w) s) >= (eval (ENum (3))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 (AAssign V_III_reorder_f
  None) 21)::(EA 21 (AAssign V_III_reorder_w (Some (ENum (0)))) 22)::
  (EA 22 (AAssign V_III_reorder_l (Some (EMul (ENum (18))
  (EVar V_III_reorder_sb)))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 25)::
  (EA 25 (AGuard (fun s => ((eval (EVar V_III_reorder_l) s) <
  (eval (ENum (576)) s))%Z)) 28)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_III_reorder_l) s) >= (eval (ENum (576))
  s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_III_reorder_f (Some (EAdd (EVar V_III_reorder_f) (ENum (-1))))) 30)::
  (EA 30 AWeaken 31)::(EA 31 (AGuard (fun s => ((eval (EVar V_III_reorder_f)
  s) = (eval (ENum (0)) s))%Z)) 33)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_III_reorder_f) s) <> (eval (ENum (0))
  s))%Z)) 32)::(EA 32 AWeaken 38)::(EA 33 AWeaken 34)::(EA 34 (AAssign
  V_III_reorder_f None) 35)::(EA 35 (AAssign V_III_reorder_w None) 36)::
  (EA 36 ANone 37)::(EA 37 AWeaken 38)::(EA 38 ANone 39)::(EA 38 ANone 40)::
  (EA 39 ANone 40)::(EA 40 ANone 41)::(EA 41 (AAssign V_III_reorder_l
  (Some (EAdd (EVar V_III_reorder_l) (ENum (1))))) 42)::(EA 42 ANone 43)::
  (EA 43 ANone 44)::(EA 44 (AAssign V_III_reorder_z (Some (EAdd (ENum (1))
  (EVar V_III_reorder_z)))) 45)::(EA 45 AWeaken 25)::(EA 46 AWeaken 47)::
  (EA 47 ANone 48)::(EA 48 (AAssign V_III_reorder_w
  (Some (EAdd (EVar V_III_reorder_w) (ENum (1))))) 49)::(EA 49 ANone 50)::
  (EA 50 ANone 51)::(EA 51 (AAssign V_III_reorder_z (Some (EAdd (ENum (1))
  (EVar V_III_reorder_z)))) 52)::(EA 52 AWeaken 18)::(EA 53 AWeaken 54)::
  (EA 54 (AAssign V_III_reorder_l None) 55)::(EA 55 ANone 56)::
  (EA 56 ANone 57)::(EA 57 (AAssign V_III_reorder_z (Some (EAdd (ENum (1))
  (EVar V_III_reorder_z)))) 58)::(EA 58 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_reorder => Pedges_III_reorder
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_reorder => 27
     end)%positive;
  var_global := var_global
}.

Definition ai_III_reorder (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 3 => (-1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0)%Z
   | 4 => (-1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0)%Z
   | 5 => (-1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0)%Z
   | 6 => (-1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_sb <= 0)%Z
   | 7 => (-1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0)%Z
   | 8 => (-1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_sb <= 0)%Z
   | 9 => (-1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0)%Z
   | 10 => (-1 * s V_III_reorder_sb + 2 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_l <= 0)%Z
   | 11 => (-1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0)%Z
   | 12 => (-1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0)%Z
   | 13 => (-1 * s V_III_reorder_sb + 2 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l + 36 <= 0)%Z
   | 14 => (-1 * s V_III_reorder_l + 36 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0)%Z
   | 15 => (-1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 16 => (-1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_w <= 0)%Z
   | 17 => (-1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 18 => (-1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_w + -3 <= 0)%Z
   | 19 => (1 * s V_III_reorder_w + -3 <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w + 3 <= 0)%Z
   | 20 => (-1 * s V_III_reorder_w + 3 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_w + -3 <= 0)%Z
   | 21 => (1 * s V_III_reorder_w + -3 <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w + 3 <= 0)%Z
   | 22 => (-1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_w <= 0)%Z
   | 23 => (-1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -36 <= 0 /\ -1 * s V_III_reorder_l <= 0)%Z
   | 24 => (-1 * s V_III_reorder_l <= 0 /\ 1 * s V_III_reorder_l + -36 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_w <= 0)%Z
   | 25 => (-1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ 1 * s V_III_reorder_l + -576 <= 0)%Z
   | 26 => (1 * s V_III_reorder_l + -576 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l + 576 <= 0)%Z
   | 27 => (-1 * s V_III_reorder_l + 576 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ 1 * s V_III_reorder_l + -576 <= 0)%Z
   | 28 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 29 => (1 * s V_III_reorder_l + -575 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0)%Z
   | 30 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 31 => (1 * s V_III_reorder_l + -575 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0)%Z
   | 32 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 33 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0 /\ 1 * s V_III_reorder_f <= 0 /\ -1 * s V_III_reorder_f <= 0)%Z
   | 34 => (-1 * s V_III_reorder_f <= 0 /\ 1 * s V_III_reorder_f <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0)%Z
   | 35 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 36 => (1 * s V_III_reorder_l + -575 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0)%Z
   | 37 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 38 => (1 * s V_III_reorder_l + -575 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0)%Z
   | 39 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 40 => (1 * s V_III_reorder_l + -575 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0)%Z
   | 41 => (1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -575 <= 0)%Z
   | 42 => (-1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_l + 1 <= 0 /\ 1 * s V_III_reorder_l + -576 <= 0)%Z
   | 43 => (1 * s V_III_reorder_l + -576 <= 0 /\ -1 * s V_III_reorder_l + 1 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 44 => (-1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_l + 1 <= 0 /\ 1 * s V_III_reorder_l + -576 <= 0)%Z
   | 45 => (1 * s V_III_reorder_l + -576 <= 0 /\ -1 * s V_III_reorder_l + 1 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_z + 1 <= 0)%Z
   | 46 => (-1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_w + -2 <= 0)%Z
   | 47 => (1 * s V_III_reorder_w + -2 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0)%Z
   | 48 => (-1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_w + -2 <= 0)%Z
   | 49 => (-1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_w + 1 <= 0 /\ 1 * s V_III_reorder_w + -3 <= 0)%Z
   | 50 => (1 * s V_III_reorder_w + -3 <= 0 /\ -1 * s V_III_reorder_w + 1 <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 51 => (-1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_w + 1 <= 0 /\ 1 * s V_III_reorder_w + -3 <= 0)%Z
   | 52 => (1 * s V_III_reorder_w + -3 <= 0 /\ -1 * s V_III_reorder_w + 1 <= 0 /\ -1 * s V_III_reorder_l <= 0 /\ -1 * s V_III_reorder_sb <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_z + 1 <= 0)%Z
   | 53 => (-1 * s V_III_reorder_sb + 2 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ 1 * s V_III_reorder_l + -35 <= 0)%Z
   | 54 => (1 * s V_III_reorder_l + -35 <= 0 /\ -1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0)%Z
   | 55 => (-1 * s V_III_reorder_sb + 2 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 56 => (-1 * s V_III_reorder_z <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0)%Z
   | 57 => (-1 * s V_III_reorder_sb + 2 <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_w <= 0 /\ -1 * s V_III_reorder_z <= 0)%Z
   | 58 => (-1 * s V_III_reorder_w <= 0 /\ 1 * s V_III_reorder_sb + -2 <= 0 /\ -1 * s V_III_reorder_sb + 2 <= 0 /\ -1 * s V_III_reorder_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_reorder (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((579 # 1) <= z)%Q
   | 2 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 3 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 4 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 5 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 6 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 7 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 8 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 9 => ((49088 # 85) + (0 # 1) * s V_III_reorder_sb + s V_III_reorder_z
           + (3 # 85) * max0(-2 + s V_III_reorder_sb)
           + (3 # 85) * max0(2 - s V_III_reorder_sb)
           + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
           + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
           + (0 # 1) * max0(576 - 18 * s V_III_reorder_sb)
           - (0 # 1) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 10 => ((579 # 1)
            - (0 # 1) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (0 # 1) * s V_III_reorder_sb * max0(35 - s V_III_reorder_l)
            + (0 # 1) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + s V_III_reorder_z
            + (0 # 1) * max0(-2 + s V_III_reorder_sb) * max0(575
                                                             - s V_III_reorder_l)
            + (0 # 1) * max0(2 - s V_III_reorder_sb) * max0(575
                                                            - s V_III_reorder_l)
            - (0 # 1) * max0(35 - s V_III_reorder_l)
            + (0 # 1) * max0(35 - s V_III_reorder_l) * max0(575
                                                            - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - s V_III_reorder_l)
            - (0 # 1) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
            - (0 # 1) * max0(576 - s V_III_reorder_l)^2
            + (0 # 1) * max0(576 - 18 * s V_III_reorder_sb) <= z)%Q
   | 11 => hints
     [(*-4.50374e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)))]
     ((579 # 1) - (0 # 1) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
      + (0 # 1) * s V_III_reorder_sb * max0(35 - s V_III_reorder_l)
      + (0 # 1) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + s V_III_reorder_z
      + (0 # 1) * max0(-2 + s V_III_reorder_sb) * max0(575
                                                       - s V_III_reorder_l)
      + (0 # 1) * max0(2 - s V_III_reorder_sb) * max0(575 - s V_III_reorder_l)
      - (0 # 1) * max0(35 - s V_III_reorder_l)
      + (0 # 1) * max0(35 - s V_III_reorder_l) * max0(575
                                                      - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(575 - s V_III_reorder_l)
      - (0 # 1) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
      - (0 # 1) * max0(576 - s V_III_reorder_l)^2
      + (0 # 1) * max0(576 - 18 * s V_III_reorder_sb) <= z)%Q
   | 12 => ((579 # 1)
            + (0 # 1) * s V_III_reorder_sb * max0(35 - s V_III_reorder_l)
            + s V_III_reorder_z
            + (0 # 1) * max0(-2 + s V_III_reorder_sb) * max0(575
                                                             - s V_III_reorder_l)
            + (0 # 1) * max0(2 - s V_III_reorder_sb) * max0(575
                                                            - s V_III_reorder_l)
            - (0 # 1) * max0(35 - s V_III_reorder_l)
            + (0 # 1) * max0(35 - s V_III_reorder_l) * max0(575
                                                            - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - s V_III_reorder_l)^2
            - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2 <= z)%Q
   | 13 => hints
     [(*-4.50374e-07 0*) F_binom_monotonic 2 (F_max0_ge_0 (575
                                                           - s V_III_reorder_l)) (F_check_ge (0) (0));
      (*-6.12101e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-6.13511e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 3.43572e-06*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-4.91845e-06 0*) F_binom_monotonic 2 (F_max0_ge_0 (575
                                                           - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0))]
     ((579 # 1) + (0 # 1) * s V_III_reorder_sb * max0(35 - s V_III_reorder_l)
      + s V_III_reorder_z
      + (0 # 1) * max0(-2 + s V_III_reorder_sb) * max0(575
                                                       - s V_III_reorder_l)
      + (0 # 1) * max0(2 - s V_III_reorder_sb) * max0(575 - s V_III_reorder_l)
      - (0 # 1) * max0(35 - s V_III_reorder_l)
      + (0 # 1) * max0(35 - s V_III_reorder_l) * max0(575
                                                      - 18 * s V_III_reorder_sb)
      - (0 # 1) * max0(575 - s V_III_reorder_l)
      + (0 # 1) * max0(575 - s V_III_reorder_l)^2
      - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2 <= z)%Q
   | 14 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 15 => ((579 # 1) + s V_III_reorder_z <= z)%Q
   | 16 => ((576 # 1) + s V_III_reorder_z + max0(3 - s V_III_reorder_w) <= z)%Q
   | 17 => hints
     [(*-0.0184755 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)))]
     ((576 # 1) + s V_III_reorder_z + max0(3 - s V_III_reorder_w) <= z)%Q
   | 18 => ((576 # 1)
            - (1 # 3) * s V_III_reorder_sb * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z + max0(3 - s V_III_reorder_w)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 19 => hints
     [(*0 0.0323994*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.00202849*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_III_reorder_w)) (F_check_ge (0) (0))]
     ((576 # 1)
      - (1 # 3) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      + s V_III_reorder_z + max0(3 - s V_III_reorder_w)
      + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 20 => ((576 # 1)
            - (51 # 58) * s V_III_reorder_sb * max0(576
                                                    - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (5 # 154) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   18 * s V_III_reorder_sb)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 21 => ((576 # 1)
            - (51 # 58) * s V_III_reorder_sb * max0(576
                                                    - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (5 # 154) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   18 * s V_III_reorder_sb)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 22 => ((576 # 1)
            - (51 # 58) * s V_III_reorder_sb * max0(576
                                                    - 18 * s V_III_reorder_sb)
            + (1007683 # 85) * s V_III_reorder_z
            - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            - (5 # 154) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   18 * s V_III_reorder_sb)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            - (1007598 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 23 => ((576 # 1) + (0 # 1) * s V_III_reorder_l
            + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
            - (26 # 49) * s V_III_reorder_l * max0(-576
                                                   + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (8 # 15) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
            - (2 # 33) * s V_III_reorder_l * max0(36
                                                  - 18 * s V_III_reorder_sb)
            + (55 # 103) * s V_III_reorder_l * max0(575
                                                    - 18 * s V_III_reorder_sb)
            - (8 # 15) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            - (1 # 2) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (0 # 1) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (11 # 31) * s V_III_reorder_sb * max0(-1 + s V_III_reorder_l)
            + (1133 # 106) * s V_III_reorder_sb * max0(36
                                                       - 18 * s V_III_reorder_sb)
            - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (619 # 71) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (211 # 46) * s V_III_reorder_sb * max0(s V_III_reorder_l)
            + (0 # 1) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                                - s V_III_reorder_l)
            - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (8 # 15) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (2457 # 128) * max0(36 - s V_III_reorder_l)
            - (2 # 33) * max0(36 - s V_III_reorder_l) * max0(36
                                                             - 18 * s V_III_reorder_sb)
            - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (8 # 15) * max0(36 - s V_III_reorder_l)^2
            - (2457 # 128) * max0(36 - 18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (26142 # 85) * max0(575 - s V_III_reorder_l)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (26142 # 85) * max0(575 - 18 * s V_III_reorder_sb)
            + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (25 # 117) * max0(576 - 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (21529 # 131) * max0(s V_III_reorder_l)
            - (21529 # 131) * max0(18 * s V_III_reorder_sb)
            - (1007598 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 24 => hints
     [(*-0.533202 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (36
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l));
      (*-0.0606121 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.500803 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.533131 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.46858 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.000819164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.213748 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.46858 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)))]
     ((576 # 1) + (0 # 1) * s V_III_reorder_l
      + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
      - (26 # 49) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (8 # 15) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
      - (2 # 33) * s V_III_reorder_l * max0(36 - 18 * s V_III_reorder_sb)
      + (55 # 103) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (8 # 15) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      - (1 # 2) * s V_III_reorder_l * max0(s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (0 # 1) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (11 # 31) * s V_III_reorder_sb * max0(-1 + s V_III_reorder_l)
      + (1133 # 106) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (619 # 71) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      - (211 # 46) * s V_III_reorder_sb * max0(s V_III_reorder_l)
      + (0 # 1) * s V_III_reorder_sb^2 + (1007683 # 85) * s V_III_reorder_z
      - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                          - s V_III_reorder_l)
      - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (8 # 15) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                               - s V_III_reorder_l)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
      + (2457 # 128) * max0(36 - s V_III_reorder_l)
      - (2 # 33) * max0(36 - s V_III_reorder_l) * max0(36
                                                       - 18 * s V_III_reorder_sb)
      - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      - (8 # 15) * max0(36 - s V_III_reorder_l)^2
      - (2457 # 128) * max0(36 - 18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (26142 # 85) * max0(575 - s V_III_reorder_l)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (26142 # 85) * max0(575 - 18 * s V_III_reorder_sb)
      + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (25 # 117) * max0(576 - 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (21529 # 131) * max0(s V_III_reorder_l)
      - (21529 # 131) * max0(18 * s V_III_reorder_sb)
      - (1007598 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 25 => (-(5846 # 61) + (265 # 7) * s V_III_reorder_l
            + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (8 # 15) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
            + (1 # 1) * s V_III_reorder_l * max0(575
                                                 - 18 * s V_III_reorder_sb)
            - (41 # 77) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (8 # 15) * s V_III_reorder_l^2 - (0 # 1) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (11 # 31) * s V_III_reorder_sb * max0(-1 + s V_III_reorder_l)
            + (1133 # 106) * s V_III_reorder_sb * max0(36
                                                       - 18 * s V_III_reorder_sb)
            - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (619 # 71) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            + (0 # 1) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                                - s V_III_reorder_l)
            - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (33414 # 109) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1381 # 74) * max0(36 - s V_III_reorder_l)
            - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            + (1 # 2) * max0(36 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (1133 # 53) * max0(36 - 18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (26142 # 85) * max0(575 - s V_III_reorder_l)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (26142 # 85) * max0(575 - 18 * s V_III_reorder_sb)
            + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (42 # 89) * max0(576 - s V_III_reorder_l)
            - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(576 - s V_III_reorder_l)^2
            - (21529 # 131) * max0(18 * s V_III_reorder_sb)
            - (1007598 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 26 => hints
     [(*-0.0018448 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-576
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l));
      (*-0.0018448 0*) F_binom_monotonic 2 (F_max0_ge_0 (-576
                                                         + s V_III_reorder_l)) (F_check_ge (0) (0));
      (*-0.535047 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                          + s V_III_reorder_l)) (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0));
      (*-2.83064e-05 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_III_reorder_sb) (0))) (F_max0_ge_0 (s V_III_reorder_sb));
      (*-0.533131 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-576
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.531392*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-576
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.535047 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-576
                                                                    + 
                                                                    s V_III_reorder_l)) (F_check_ge (-576
                                                                    + s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.535047 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-576
                                                                    + 
                                                                    s V_III_reorder_l)) (F_check_ge (-576
                                                                    + s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.00173913 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 
                                                                    s V_III_reorder_l)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.535047 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.535047 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.469395 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0018448 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.471134 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.00173913*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.593814*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.469395 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.535975 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.00284168 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.532206 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.0323994*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.246891*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.000819164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.0018448 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.533946 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-5.29245e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.531445 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.019716 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.500803 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.449679 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.0516186*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.00173904 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-5927.05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_z)) (F_check_ge (s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_w)) (F_check_ge (0) (0)));
      (*-11854.1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z));
      (*-37.2149 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + 
                                                                    s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l));
      (*-1.16437 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-576
                                                                    + 
                                                                    s V_III_reorder_l) (0))) (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l));
      (*-2.83062e-05 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_III_reorder_sb)) (F_check_ge (0) (0))]
     (-(5846 # 61) + (265 # 7) * s V_III_reorder_l
      + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
      - (117 # 110) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (8 # 15) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
      + (1 # 1) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (41 # 77) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (8 # 15) * s V_III_reorder_l^2 - (0 # 1) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (11 # 31) * s V_III_reorder_sb * max0(-1 + s V_III_reorder_l)
      + (1133 # 106) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (619 # 71) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      + (0 # 1) * s V_III_reorder_sb^2 + (1007683 # 85) * s V_III_reorder_z
      - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                          - s V_III_reorder_l)
      - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (33414 # 109) * max0(-576 + 18 * s V_III_reorder_sb)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
      + (1381 # 74) * max0(36 - s V_III_reorder_l)
      - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      + (1 # 2) * max0(36 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (1133 # 53) * max0(36 - 18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (26142 # 85) * max0(575 - s V_III_reorder_l)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (26142 # 85) * max0(575 - 18 * s V_III_reorder_sb)
      + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      - (42 # 89) * max0(576 - s V_III_reorder_l)
      - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (0 # 1) * max0(576 - s V_III_reorder_l)^2
      - (21529 # 131) * max0(18 * s V_III_reorder_sb)
      - (1007598 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 27 => (s V_III_reorder_z <= z)%Q
   | 28 => hints
     [(*0 0.330786*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb));
      (*0 0.533202*) F_binom_monotonic 2 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0));
      (*0 0.252356*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.330786 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.272018*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.535975*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.532206*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.501788*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 41.2299*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                   - 
                                                                   s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*-0.0323994 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.0170011*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.0351793*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.00086885*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.0170011*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.019716*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 329.253*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*0 329.253*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_z)) (F_check_ge (s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 9.8212*) F_binom_monotonic 1 (F_max0_ge_arg (576
                                                       - s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))]
     (-(5846 # 61) + (265 # 7) * s V_III_reorder_l
      + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
      - (117 # 110) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (8 # 15) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
      + (1 # 1) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (41 # 77) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (8 # 15) * s V_III_reorder_l^2 - (0 # 1) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (11 # 31) * s V_III_reorder_sb * max0(-1 + s V_III_reorder_l)
      + (1133 # 106) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (619 # 71) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      + (0 # 1) * s V_III_reorder_sb^2 + (1007683 # 85) * s V_III_reorder_z
      - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                          - s V_III_reorder_l)
      - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (33414 # 109) * max0(-576 + 18 * s V_III_reorder_sb)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
      + (1381 # 74) * max0(36 - s V_III_reorder_l)
      - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      + (1 # 2) * max0(36 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (1133 # 53) * max0(36 - 18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (26142 # 85) * max0(575 - s V_III_reorder_l)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (26142 # 85) * max0(575 - 18 * s V_III_reorder_sb)
      + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      - (42 # 89) * max0(576 - s V_III_reorder_l)
      - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (0 # 1) * max0(576 - s V_III_reorder_l)^2
      - (21529 # 131) * max0(18 * s V_III_reorder_sb)
      - (1007598 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 29 => ((66877 # 13) + (4538 # 165) * s V_III_reorder_l
            + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
            - (233 # 146) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (1 # 2) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (8 # 15) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            - (1 # 2) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3587 # 87) * s V_III_reorder_l * max0(s V_III_reorder_z)
            + (41429 # 98) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (535 # 113) * s V_III_reorder_sb * max0(36
                                                      - 18 * s V_III_reorder_sb)
            - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (268 # 59) * s V_III_reorder_sb * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (816 # 97) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (519 # 106) * s V_III_reorder_sb * max0(576
                                                      - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                                - s V_III_reorder_l)
            - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1 # 2) * max0(36 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (49 # 5) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            - (31 # 114) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (26142 # 85) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (3587 # 87) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   s V_III_reorder_l)
            + (0 # 1) * max0(576 - s V_III_reorder_l)
            - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (0 # 1) * max0(576 - s V_III_reorder_l)^2
            + (18789 # 65) * max0(s V_III_reorder_l)
            - (8 # 15) * max0(s V_III_reorder_l)^2
            - (21529 # 131) * max0(18 * s V_III_reorder_sb)
            + (1007513 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 30 => hints
     [(*-4.9686e-05 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l));
      (*0 0.000809986*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.502657 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.228713 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.532206 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.0305949 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.0201348 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.24536 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.265495 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 5.29244e-05*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.228713*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.500803*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.502657*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.310652*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)))]
     ((66877 # 13) + (4538 # 165) * s V_III_reorder_l
      + (33 # 62) * s V_III_reorder_l * max0(-576 + s V_III_reorder_l)
      - (233 # 146) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (1 # 2) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
      + (221 # 147) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (8 # 15) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (1 # 59) * s V_III_reorder_l * max0(576 - 18 * s V_III_reorder_sb)
      - (1 # 2) * s V_III_reorder_l * max0(s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (3587 # 87) * s V_III_reorder_l * max0(s V_III_reorder_z)
      + (41429 # 98) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (535 # 113) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (268 # 59) * s V_III_reorder_sb * max0(575 - 18 * s V_III_reorder_sb)
      + (816 # 97) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      - (519 # 106) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
      - (11039 # 103) * s V_III_reorder_sb^2
      + (1007683 # 85) * s V_III_reorder_z
      - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
      + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      + (33 # 62) * max0(-576 + s V_III_reorder_l) * max0(576
                                                          - s V_III_reorder_l)
      - (0 # 1) * max0(-576 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                - s V_III_reorder_l)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
      + (1 # 2) * max0(36 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (49 # 5) * max0(36 - 18 * s V_III_reorder_sb)
      + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                               - 18 * s V_III_reorder_sb)
      - (31 # 114) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                               - 18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (26142 # 85) * max0(575 - s V_III_reorder_l)
      + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                          - 18 * s V_III_reorder_sb)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (3587 # 87) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
      - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
      + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                             - s V_III_reorder_l)
      + (0 # 1) * max0(576 - s V_III_reorder_l)
      - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (0 # 1) * max0(576 - s V_III_reorder_l)^2
      + (18789 # 65) * max0(s V_III_reorder_l)
      - (8 # 15) * max0(s V_III_reorder_l)^2
      - (21529 # 131) * max0(18 * s V_III_reorder_sb)
      + (1007513 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 31 => ((71791 # 14) + (4575 # 166) * s V_III_reorder_l
            - (233 # 146) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3587 # 87) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (0 # 1) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (62 # 195) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (268 # 59) * s V_III_reorder_sb * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (467 # 103) * s V_III_reorder_sb * max0(576
                                                      - 18 * s V_III_reorder_sb)
            - (1203 # 116) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
            + (33414 # 109) * max0(-576 + s V_III_reorder_l)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (11969 # 91) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (42598 # 131) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (3587 # 87) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   s V_III_reorder_l)
            - (34 # 73) * max0(576 - s V_III_reorder_l)
            + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (38 # 81) * max0(s V_III_reorder_l)
            + (35 # 153) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (8 # 15) * max0(s V_III_reorder_l)^2
            - (12741 # 89) * max0(18 * s V_III_reorder_sb)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2
            + (1007513 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 32 => hints
     [(*-270.365 0*) F_max0_pre_decrement 1 (576 - s V_III_reorder_l) (1);
      (*-0.000819164 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l));
      (*-0.311125 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0305949 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-20.6141 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*-0.0323994 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.248429*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.533131 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.265022 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.501605 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.532206 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.517377 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.484985 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.517377 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.019716 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.248429 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.531392 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.0531563 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.265495 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.265022 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0334403 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-20.6158 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-20.6158 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-329.253 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-5927.05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_z)) (F_check_ge (s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_w)) (F_check_ge (0) (0)));
      (*-7.75552 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0));
      (*-0.533202 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))]
     ((71791 # 14) + (4575 # 166) * s V_III_reorder_l
      - (233 # 146) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
      + (221 # 147) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (1 # 59) * s V_III_reorder_l * max0(576 - 18 * s V_III_reorder_sb)
      + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (3587 # 87) * s V_III_reorder_l * max0(s V_III_reorder_z)
      - (0 # 1) * s V_III_reorder_l^2 + (41429 # 98) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (62 # 195) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (268 # 59) * s V_III_reorder_sb * max0(575 - 18 * s V_III_reorder_sb)
      + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      - (467 # 103) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      - (1203 # 116) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
      - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
      - (11039 # 103) * s V_III_reorder_sb^2
      + (1007683 # 85) * s V_III_reorder_z
      - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
      + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
      + (33414 # 109) * max0(-576 + s V_III_reorder_l)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                - s V_III_reorder_l)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
      + (11969 # 91) * max0(36 - 18 * s V_III_reorder_sb)
      + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                               - 18 * s V_III_reorder_sb)
      - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                               - 18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (42598 # 131) * max0(575 - s V_III_reorder_l)
      + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                          - 18 * s V_III_reorder_sb)
      - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (3587 # 87) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
      - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
      + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                             - s V_III_reorder_l)
      - (34 # 73) * max0(576 - s V_III_reorder_l)
      + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
      - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
      + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
      - (38 # 81) * max0(s V_III_reorder_l)
      + (35 # 153) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (8 # 15) * max0(s V_III_reorder_l)^2
      - (12741 # 89) * max0(18 * s V_III_reorder_sb)
      + (32 # 103) * max0(18 * s V_III_reorder_sb)^2
      + (1007513 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 33 => hints
     [(*0 0.576146*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-20.6141 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*0 0.000819164*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.501605 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.501605 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.195273*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.248429 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-5927.05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_z)) (F_check_ge (s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_w)) (F_check_ge (0) (0)));
      (*-8.69592 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))]
     ((71791 # 14) + (4575 # 166) * s V_III_reorder_l
      - (233 # 146) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
      + (221 # 147) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (1 # 59) * s V_III_reorder_l * max0(576 - 18 * s V_III_reorder_sb)
      + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (3587 # 87) * s V_III_reorder_l * max0(s V_III_reorder_z)
      - (0 # 1) * s V_III_reorder_l^2 + (41429 # 98) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (62 # 195) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (173 # 18) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (268 # 59) * s V_III_reorder_sb * max0(575 - 18 * s V_III_reorder_sb)
      + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      - (467 # 103) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      - (1203 # 116) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
      - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
      - (11039 # 103) * s V_III_reorder_sb^2
      + (1007683 # 85) * s V_III_reorder_z
      - (503799 # 85) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
      + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
      + (33414 # 109) * max0(-576 + s V_III_reorder_l)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                - s V_III_reorder_l)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      + (503799 # 85) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
      + (11969 # 91) * max0(36 - 18 * s V_III_reorder_sb)
      + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                               - 18 * s V_III_reorder_sb)
      - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                               - 18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (42598 # 131) * max0(575 - s V_III_reorder_l)
      + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                          - 18 * s V_III_reorder_sb)
      - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (3587 # 87) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
      - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
      + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                             - s V_III_reorder_l)
      - (34 # 73) * max0(576 - s V_III_reorder_l)
      + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
      - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
      + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
      - (38 # 81) * max0(s V_III_reorder_l)
      + (35 # 153) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (8 # 15) * max0(s V_III_reorder_l)^2
      - (12741 # 89) * max0(18 * s V_III_reorder_sb)
      + (32 # 103) * max0(18 * s V_III_reorder_sb)^2
      + (1007513 # 85) * max0(s V_III_reorder_z) <= z)%Q
   | 34 => ((71791 # 14) + (2973 # 82) * s V_III_reorder_l
            - (233 # 146) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            + (1 # 2) * s V_III_reorder_l * max0(36 - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (0 # 1) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (62 # 195) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (317 # 52) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (268 # 59) * s V_III_reorder_sb * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (467 # 103) * s V_III_reorder_sb * max0(576
                                                      - 18 * s V_III_reorder_sb)
            + (27 # 2) * s V_III_reorder_sb * max0(s V_III_reorder_l)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
            + (33414 # 109) * max0(-576 + s V_III_reorder_l)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (10703 # 68) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (1 # 2) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                  - s V_III_reorder_l)
            - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (53 # 92) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (42598 # 131) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (93 # 185) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   s V_III_reorder_l)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            - (34 # 73) * max0(576 - s V_III_reorder_l)
            + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (22878 # 77) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (8 # 15) * max0(s V_III_reorder_l)^2
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 35 => ((71791 # 14) + (2973 # 82) * s V_III_reorder_l
            - (233 # 146) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            + (1 # 2) * s V_III_reorder_l * max0(36 - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (0 # 1) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (62 # 195) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (317 # 52) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (268 # 59) * s V_III_reorder_sb * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (467 # 103) * s V_III_reorder_sb * max0(576
                                                      - 18 * s V_III_reorder_sb)
            + (27 # 2) * s V_III_reorder_sb * max0(s V_III_reorder_l)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
            + (33414 # 109) * max0(-576 + s V_III_reorder_l)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (10703 # 68) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (1 # 2) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                  - s V_III_reorder_l)
            - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (53 # 92) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (42598 # 131) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (93 # 185) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   s V_III_reorder_l)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            - (34 # 73) * max0(576 - s V_III_reorder_l)
            + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (22878 # 77) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (8 # 15) * max0(s V_III_reorder_l)^2
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 36 => ((71791 # 14) + (2973 # 82) * s V_III_reorder_l
            - (233 # 146) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            + (1 # 2) * s V_III_reorder_l * max0(36 - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (0 # 1) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
            + (62 # 195) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (317 # 52) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            + (268 # 59) * s V_III_reorder_sb * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (467 # 103) * s V_III_reorder_sb * max0(576
                                                      - 18 * s V_III_reorder_sb)
            + (27 # 2) * s V_III_reorder_sb * max0(s V_III_reorder_l)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
            + (33414 # 109) * max0(-576 + s V_III_reorder_l)
            - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                               - 18 * s V_III_reorder_sb)
            - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (10703 # 68) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (1 # 2) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                  - s V_III_reorder_l)
            - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (53 # 92) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (42598 # 131) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                                - 18 * s V_III_reorder_sb)
            - (93 # 185) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                   - 
                                                                   s V_III_reorder_l)
            + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            - (34 # 73) * max0(576 - s V_III_reorder_l)
            + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (22878 # 77) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            - (8 # 15) * max0(s V_III_reorder_l)^2
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 37 => hints
     [(*-0.534022 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l));
      (*-0.236556 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.517377 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.252356 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.501605 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.265049 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.000819164 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.0305949 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.484985 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0323994 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.236556 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.533131 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.252356 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.517377 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.532206 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.501605 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.484985 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.265049*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.000819164*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.484985*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.019716 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.501605*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.000819164 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.531392 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 0.265495*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.265022 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0334403 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 20.6158*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 20.6158*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 329.253*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)))]
     ((71791 # 14) + (2973 # 82) * s V_III_reorder_l
      - (233 # 146) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      - (2 # 101) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      + (1 # 2) * s V_III_reorder_l * max0(36 - 18 * s V_III_reorder_sb)
      - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
      + (221 # 147) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (1 # 59) * s V_III_reorder_l * max0(576 - 18 * s V_III_reorder_sb)
      + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
      - (0 # 1) * s V_III_reorder_l^2 + (41429 # 98) * s V_III_reorder_sb
      - (1 # 32) * s V_III_reorder_sb * max0(-576 + s V_III_reorder_l)
      + (62 # 195) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (317 # 52) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
      + (268 # 59) * s V_III_reorder_sb * max0(575 - 18 * s V_III_reorder_sb)
      + (750 # 89) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      - (467 # 103) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      + (27 # 2) * s V_III_reorder_sb * max0(s V_III_reorder_l)
      - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
      - (11039 # 103) * s V_III_reorder_sb^2
      + (1007683 # 85) * s V_III_reorder_z
      + (31279 # 95) * s V_III_reorder_z * max0(18 * s V_III_reorder_sb)
      + (33414 # 109) * max0(-576 + s V_III_reorder_l)
      - (8 # 15) * max0(-576 + s V_III_reorder_l) * max0(575
                                                         - 18 * s V_III_reorder_sb)
      - (76 # 143) * max0(-576 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (31241 # 51) * max0(-576 + 18 * s V_III_reorder_sb)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                - s V_III_reorder_l)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      + (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (10703 # 68) * max0(36 - 18 * s V_III_reorder_sb)
      + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                               - 18 * s V_III_reorder_sb)
      + (1 # 2) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                            - s V_III_reorder_l)
      - (89 # 172) * max0(36 - 18 * s V_III_reorder_sb) * max0(576
                                                               - 18 * s V_III_reorder_sb)
      + (53 # 92) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      + (42598 # 131) * max0(575 - s V_III_reorder_l)
      + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                          - 18 * s V_III_reorder_sb)
      - (3 # 98) * max0(575 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      - (55 # 103) * max0(575 - s V_III_reorder_l) * max0(576
                                                          - 18 * s V_III_reorder_sb)
      - (93 # 185) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
      - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (3113 # 151) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_z)
      - (81011 # 134) * max0(575 - 18 * s V_III_reorder_sb)
      + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                             - s V_III_reorder_l)
      + (1 # 2) * max0(575 - 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
      - (34 # 73) * max0(576 - s V_III_reorder_l)
      + (6 # 179) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
      - (30 # 113) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
      + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
      - (22878 # 77) * max0(s V_III_reorder_l)
      - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      - (8 # 15) * max0(s V_III_reorder_l)^2
      - (11309 # 69) * max0(18 * s V_III_reorder_sb)
      + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 38 => ((71791 # 14) + (2980 # 81) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (5 # 154) * s V_III_reorder_l * max0(35 - s V_III_reorder_l)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(575 - s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(s V_III_reorder_l)
            + (16831 # 55) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (503 # 27) * max0(35 - s V_III_reorder_l)
            - (5 # 154) * max0(35 - s V_III_reorder_l) * max0(575
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (21817 # 69) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (5 # 102) * max0(575 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - s V_III_reorder_l)^2
            - (33414 # 109) * max0(575 - 18 * s V_III_reorder_sb)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (1121 # 128) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 39 => ((71791 # 14) + (2980 # 81) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (5 # 154) * s V_III_reorder_l * max0(35 - s V_III_reorder_l)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(575 - s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(s V_III_reorder_l)
            + (16831 # 55) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (503 # 27) * max0(35 - s V_III_reorder_l)
            - (5 # 154) * max0(35 - s V_III_reorder_l) * max0(575
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (21817 # 69) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (5 # 102) * max0(575 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - s V_III_reorder_l)^2
            - (33414 # 109) * max0(575 - 18 * s V_III_reorder_sb)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (1121 # 128) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 40 => ((71791 # 14) + (2980 # 81) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (5 # 154) * s V_III_reorder_l * max0(35 - s V_III_reorder_l)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(575 - s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(s V_III_reorder_l)
            + (16831 # 55) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (503 # 27) * max0(35 - s V_III_reorder_l)
            - (5 # 154) * max0(35 - s V_III_reorder_l) * max0(575
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (21817 # 69) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (5 # 102) * max0(575 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - s V_III_reorder_l)^2
            - (33414 # 109) * max0(575 - 18 * s V_III_reorder_sb)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (1121 # 128) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 41 => ((71791 # 14) + (2980 # 81) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            - (5 # 154) * s V_III_reorder_l * max0(35 - s V_III_reorder_l)
            - (3 # 98) * s V_III_reorder_l * max0(575 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(s V_III_reorder_l)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(575 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(575 - s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(s V_III_reorder_l)
            + (16831 # 55) * max0(-576 + 18 * s V_III_reorder_sb)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(575
                                                                    - s V_III_reorder_l)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(s V_III_reorder_l)
            + (503 # 27) * max0(35 - s V_III_reorder_l)
            - (5 # 154) * max0(35 - s V_III_reorder_l) * max0(575
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            + (21817 # 69) * max0(575 - s V_III_reorder_l)
            + (67 # 125) * max0(575 - s V_III_reorder_l) * max0(575
                                                                - 18 * s V_III_reorder_sb)
            - (5 # 102) * max0(575 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (1 # 2) * max0(575 - s V_III_reorder_l) * max0(s V_III_reorder_l)
            - (25 # 128) * max0(575 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - s V_III_reorder_l)^2
            - (33414 # 109) * max0(575 - 18 * s V_III_reorder_sb)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            - (951 # 82) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (1121 # 128) * max0(s V_III_reorder_l)
            - (2 # 101) * max0(s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(s V_III_reorder_l) * max0(s V_III_reorder_z)
            - (11309 # 69) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 42 => ((682141 # 134) + (4543 # 120) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (5 # 154) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(-1 + s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(576 - s V_III_reorder_l)
            + (33779 # 110) * max0(-576 + 18 * s V_III_reorder_sb)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(-1
                                                                    + s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (254 # 29) * max0(-1 + s V_III_reorder_l)
            - (1 # 2) * max0(-1 + s V_III_reorder_l) * max0(576
                                                            - s V_III_reorder_l)
            - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_z)
            + (1381 # 74) * max0(36 - s V_III_reorder_l)
            - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            - (28649 # 93) * max0(575 - 18 * s V_III_reorder_sb)
            + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            + (23084 # 73) * max0(576 - s V_III_reorder_l)
            - (5 # 102) * max0(576 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(576 - s V_III_reorder_l)^2
            - (1115 # 96) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (17618 # 107) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2
            + (3113 # 151) * max0(s V_III_reorder_z) <= z)%Q
   | 43 => ((682141 # 134) + (4543 # 120) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (5 # 154) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(-1 + s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(576 - s V_III_reorder_l)
            + (33779 # 110) * max0(-576 + 18 * s V_III_reorder_sb)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(-1
                                                                    + s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (254 # 29) * max0(-1 + s V_III_reorder_l)
            - (1 # 2) * max0(-1 + s V_III_reorder_l) * max0(576
                                                            - s V_III_reorder_l)
            - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_z)
            + (1381 # 74) * max0(36 - s V_III_reorder_l)
            - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            - (28649 # 93) * max0(575 - 18 * s V_III_reorder_sb)
            + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            + (23084 # 73) * max0(576 - s V_III_reorder_l)
            - (5 # 102) * max0(576 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(576 - s V_III_reorder_l)^2
            - (1115 # 96) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (17618 # 107) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2
            + (3113 # 151) * max0(s V_III_reorder_z) <= z)%Q
   | 44 => ((682141 # 134) + (4543 # 120) * s V_III_reorder_l
            - (117 # 110) * s V_III_reorder_l * max0(-576
                                                     + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
            - (5 # 154) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
            + (221 # 147) * s V_III_reorder_l * max0(575
                                                     - 18 * s V_III_reorder_sb)
            - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
            + (1 # 59) * s V_III_reorder_l * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
            - (3113 # 151) * s V_III_reorder_l * max0(s V_III_reorder_z)
            - (47 # 88) * s V_III_reorder_l^2
            + (41429 # 98) * s V_III_reorder_sb
            + (626 # 65) * s V_III_reorder_sb * max0(36
                                                     - 18 * s V_III_reorder_sb)
            - (193 # 105) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
            - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                       - 18 * s V_III_reorder_sb)
            - (314107 # 53) * s V_III_reorder_sb * max0(s V_III_reorder_z)
            - (11039 # 103) * s V_III_reorder_sb^2
            + (1007683 # 85) * s V_III_reorder_z
            - (3113 # 151) * s V_III_reorder_z * max0(-1 + s V_III_reorder_l)
            - (3113 # 151) * s V_III_reorder_z * max0(576 - s V_III_reorder_l)
            + (33779 # 110) * max0(-576 + 18 * s V_III_reorder_sb)
            + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(-1
                                                                    + s V_III_reorder_l)
            - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (254 # 29) * max0(-1 + s V_III_reorder_l)
            - (1 # 2) * max0(-1 + s V_III_reorder_l) * max0(576
                                                            - s V_III_reorder_l)
            - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (3113 # 151) * max0(-1 + s V_III_reorder_l) * max0(s V_III_reorder_z)
            + (1381 # 74) * max0(36 - s V_III_reorder_l)
            - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                              - s V_III_reorder_l)
            - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
            + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)
            + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
            + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
            - (28649 # 93) * max0(575 - 18 * s V_III_reorder_sb)
            + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - s V_III_reorder_l)
            - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                    - 18 * s V_III_reorder_sb)
            + (23084 # 73) * max0(576 - s V_III_reorder_l)
            - (5 # 102) * max0(576 - s V_III_reorder_l) * max0(576
                                                               - 18 * s V_III_reorder_sb)
            - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
            + (0 # 1) * max0(576 - s V_III_reorder_l)^2
            - (1115 # 96) * max0(576 - 18 * s V_III_reorder_sb)
            + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
            - (17618 # 107) * max0(18 * s V_III_reorder_sb)
            + (31279 # 95) * max0(18 * s V_III_reorder_sb) * max0(s V_III_reorder_z)
            + (32 # 103) * max0(18 * s V_III_reorder_sb)^2
            + (3113 # 151) * max0(s V_III_reorder_z) <= z)%Q
   | 45 => hints
     [(*-0.0201348 0*) F_binom_monotonic 2 (F_max0_ge_arg (576
                                                           - 18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0));
      (*-0.00086885 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0));
      (*0 0.310652*) F_binom_monotonic 2 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0));
      (*-0.500795 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.501788 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-20.6158 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*-0.531445 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_III_reorder_l)) (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-20.6158 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_III_reorder_l)) (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*-0.520511 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_III_reorder_l)) (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.00086885 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (-1
                                                                    + s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.517377 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.533131 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.500795 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.252356 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.532206*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 20.6158*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*-0.537512 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    s V_III_reorder_l)) (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.520511 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.586473 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (576
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.533946 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (576
                                                                    - 
                                                                    18 * s V_III_reorder_sb)) (F_check_ge (576
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.531445 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0205848 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.500803 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.00086885 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_l) (0))) (F_max0_ge_0 (s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-5.29245e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_l)) (F_check_ge (s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-0.531392 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-576
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-329.253 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_z)) (F_check_ge (0) (0)));
      (*-0.311125 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-5927.05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_w)) (F_check_ge (0) (0)));
      (*0 20.6158*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_z)) (F_check_ge (s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 20.6158*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_III_reorder_z)) (F_check_ge (s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)))]
     (-(649391 # 96) + (4543 # 120) * s V_III_reorder_l
      - (117 # 110) * s V_III_reorder_l * max0(-576 + 18 * s V_III_reorder_sb)
      + (0 # 1) * s V_III_reorder_l * max0(-1 + s V_III_reorder_l)
      - (3113 # 151) * s V_III_reorder_l * max0(-1 + s V_III_reorder_z)
      - (5 # 154) * s V_III_reorder_l * max0(36 - s V_III_reorder_l)
      + (221 # 147) * s V_III_reorder_l * max0(575 - 18 * s V_III_reorder_sb)
      - (3 # 98) * s V_III_reorder_l * max0(576 - s V_III_reorder_l)
      + (1 # 59) * s V_III_reorder_l * max0(576 - 18 * s V_III_reorder_sb)
      + (34 # 45) * s V_III_reorder_l * max0(18 * s V_III_reorder_sb)
      - (47 # 88) * s V_III_reorder_l^2 + (41429 # 98) * s V_III_reorder_sb
      - (314107 # 53) * s V_III_reorder_sb * max0(-1 + s V_III_reorder_z)
      + (626 # 65) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (193 # 105) * s V_III_reorder_sb * max0(576 - s V_III_reorder_l)
      - (1518 # 163) * s V_III_reorder_sb * max0(576
                                                 - 18 * s V_III_reorder_sb)
      - (11039 # 103) * s V_III_reorder_sb^2
      + (1007683 # 85) * s V_III_reorder_z
      - (3113 # 151) * s V_III_reorder_z * max0(-1 + s V_III_reorder_l)
      - (3113 # 151) * s V_III_reorder_z * max0(576 - s V_III_reorder_l)
      + (33779 # 110) * max0(-576 + 18 * s V_III_reorder_sb)
      + (59 # 111) * max0(-576 + 18 * s V_III_reorder_sb) * max0(-1
                                                                 + s V_III_reorder_l)
      - (33 # 62) * max0(-576 + 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      + (83 # 7) * max0(-1 + s V_III_reorder_l)
      + (3113 # 151) * max0(-1 + s V_III_reorder_l) * max0(-1
                                                           + s V_III_reorder_z)
      - (1 # 2) * max0(-1 + s V_III_reorder_l) * max0(576 - s V_III_reorder_l)
      - (2 # 101) * max0(-1 + s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (3113 # 151) * max0(-1 + s V_III_reorder_z)
      + (31279 # 95) * max0(-1 + s V_III_reorder_z) * max0(18 * s V_III_reorder_sb)
      + (1381 # 74) * max0(36 - s V_III_reorder_l)
      - (5 # 154) * max0(36 - s V_III_reorder_l) * max0(576
                                                        - s V_III_reorder_l)
      - (13818 # 83) * max0(36 - 18 * s V_III_reorder_sb)
      + (26 # 103) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                               - 18 * s V_III_reorder_sb)
      + (14 # 45) * max0(36 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb)
      + (19 # 32) * max0(36 - 18 * s V_III_reorder_sb)^2
      - (28649 # 93) * max0(575 - 18 * s V_III_reorder_sb)
      + (67 # 125) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                - s V_III_reorder_l)
      - (89 # 172) * max0(575 - 18 * s V_III_reorder_sb) * max0(576
                                                                - 18 * s V_III_reorder_sb)
      + (34694 # 103) * max0(576 - s V_III_reorder_l)
      - (5 # 102) * max0(576 - s V_III_reorder_l) * max0(576
                                                         - 18 * s V_III_reorder_sb)
      - (25 # 128) * max0(576 - s V_III_reorder_l) * max0(18 * s V_III_reorder_sb)
      + (0 # 1) * max0(576 - s V_III_reorder_l)^2
      - (1115 # 96) * max0(576 - 18 * s V_III_reorder_sb)
      + (2 # 99) * max0(576 - 18 * s V_III_reorder_sb)^2
      - (17618 # 107) * max0(18 * s V_III_reorder_sb)
      + (32 # 103) * max0(18 * s V_III_reorder_sb)^2 <= z)%Q
   | 46 => hints
     [(*0 1*) F_max0_pre_decrement 1 (3 - s V_III_reorder_w) (1);
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_reorder_z) (0))) (F_max0_ge_0 (s V_III_reorder_z))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_w)) (F_check_ge (0) (0)))]
     ((576 # 1)
      - (1 # 3) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      + s V_III_reorder_z + max0(3 - s V_III_reorder_w)
      + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 47 => ((577 # 1)
            - (1 # 3) * s V_III_reorder_sb * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (1 # 2) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            + max0(2 - s V_III_reorder_w)
            + (1 # 2) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 48 => ((577 # 1)
            - (1 # 3) * s V_III_reorder_sb * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (1 # 2) * s V_III_reorder_z * max0(2 - s V_III_reorder_w)
            + max0(2 - s V_III_reorder_w)
            + (1 # 2) * max0(2 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 49 => ((577 # 1)
            - (1 # 3) * s V_III_reorder_sb * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (1 # 2) * s V_III_reorder_z * max0(3 - s V_III_reorder_w)
            + max0(3 - s V_III_reorder_w)
            + (1 # 2) * max0(3 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 50 => ((577 # 1)
            - (1 # 3) * s V_III_reorder_sb * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (1 # 2) * s V_III_reorder_z * max0(3 - s V_III_reorder_w)
            + max0(3 - s V_III_reorder_w)
            + (1 # 2) * max0(3 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 51 => ((577 # 1)
            - (1 # 3) * s V_III_reorder_sb * max0(576
                                                  - 18 * s V_III_reorder_sb)
            + s V_III_reorder_z
            - (1 # 2) * s V_III_reorder_z * max0(3 - s V_III_reorder_w)
            + max0(3 - s V_III_reorder_w)
            + (1 # 2) * max0(3 - s V_III_reorder_w) * max0(s V_III_reorder_z)
            + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 52 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_III_reorder_z)) (F_check_ge (-1
                                                                    + s V_III_reorder_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_III_reorder_w)) (F_check_ge (0) (0)))]
     ((576 # 1)
      - (1 # 3) * s V_III_reorder_sb * max0(576 - 18 * s V_III_reorder_sb)
      + s V_III_reorder_z
      - (1 # 2) * s V_III_reorder_z * max0(3 - s V_III_reorder_w)
      + (1 # 2) * max0(-1 + s V_III_reorder_z) * max0(3 - s V_III_reorder_w)
      + (3 # 2) * max0(3 - s V_III_reorder_w)
      + (1 # 54) * max0(576 - 18 * s V_III_reorder_sb) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 53 => hints
     [(*-3.43392e-06 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (35
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l));
      (*0 5.16547e-05*) F_binom_monotonic 2 (F_max0_ge_arg (36
                                                            - 18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0));
      (*0 4.50374e-07*) F_binom_monotonic 2 (F_max0_ge_arg (575
                                                            - s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0));
      (*2.98355e-06 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (576
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l));
      (*-3.60019e+10 -3.60019e+10*) F_binom_monotonic 2 (F_max0_ge_0 (576
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0));
      (*1.0827e+09 1.0827e+09*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-36
                                                                    + 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (-36
                                                                    + 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-6.12255e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-6.13511e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (2
                                                                    - s V_III_reorder_sb)) (F_check_ge (2
                                                                    - s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-6.13511e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (35
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 3.43392e-06*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (35
                                                                    - s V_III_reorder_l)) (F_check_ge (35
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-3.43392e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-5.16547e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-3.05969e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-4.8595e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (36
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 3.43392e-06*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-6.13511e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - s V_III_reorder_l)) (F_check_ge (575
                                                                    - s V_III_reorder_l) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 1.91896e-07*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 2.86779e-06*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-3.43572e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-2.86779e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.00152368 0*) F_binom_monotonic 1 (F_max0_ge_arg (18 * s V_III_reorder_sb)) (F_check_ge (18 * s V_III_reorder_sb) (0));
      (*-0.00371461 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (35
                                                                    - s V_III_reorder_l) (0))) (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l));
      (*0 1.91896e-07*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))]
     ((579 # 1) + (0 # 1) * s V_III_reorder_sb * max0(35 - s V_III_reorder_l)
      + s V_III_reorder_z
      + (0 # 1) * max0(-2 + s V_III_reorder_sb) * max0(575
                                                       - s V_III_reorder_l)
      + (0 # 1) * max0(2 - s V_III_reorder_sb) * max0(575 - s V_III_reorder_l)
      - (0 # 1) * max0(35 - s V_III_reorder_l)
      + (0 # 1) * max0(35 - s V_III_reorder_l) * max0(575
                                                      - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(575 - s V_III_reorder_l)^2
      - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2 <= z)%Q
   | 54 => ((580 # 1) - (6 # 173) * s V_III_reorder_sb
            - (0 # 1) * s V_III_reorder_sb * max0(-1
                                                  + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_sb * max0(36
                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
            + (1 # 60) * s V_III_reorder_sb^2 + s V_III_reorder_z
            + (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb) * max0(36
                                                                  - 18 * s V_III_reorder_sb)
            + (4 # 121) * max0(2 - s V_III_reorder_sb)
            - (0 # 1) * max0(36 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(575 - s V_III_reorder_fresh_dot_4)
            - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
            + (0 # 1) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 55 => ((580 # 1) - (6 # 173) * s V_III_reorder_sb
            - (0 # 1) * s V_III_reorder_sb * max0(-1
                                                  + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_sb * max0(36
                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
            + (1 # 60) * s V_III_reorder_sb^2 + s V_III_reorder_z
            + (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb) * max0(36
                                                                  - 18 * s V_III_reorder_sb)
            + (4 # 121) * max0(2 - s V_III_reorder_sb)
            - (0 # 1) * max0(36 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(575 - s V_III_reorder_l)
            - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
            + (0 # 1) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 56 => ((580 # 1) - (6 # 173) * s V_III_reorder_sb
            - (0 # 1) * s V_III_reorder_sb * max0(-1
                                                  + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_sb * max0(36
                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
            + (1 # 60) * s V_III_reorder_sb^2 + s V_III_reorder_z
            + (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb) * max0(36
                                                                  - 18 * s V_III_reorder_sb)
            + (4 # 121) * max0(2 - s V_III_reorder_sb)
            - (0 # 1) * max0(36 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(575 - s V_III_reorder_l)
            - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
            + (0 # 1) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 57 => ((580 # 1) - (6 # 173) * s V_III_reorder_sb
            - (0 # 1) * s V_III_reorder_sb * max0(-1
                                                  + 18 * s V_III_reorder_sb)
            + (0 # 1) * s V_III_reorder_sb * max0(36
                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
            + (1 # 60) * s V_III_reorder_sb^2 + s V_III_reorder_z
            + (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb) * max0(36
                                                                  - 18 * s V_III_reorder_sb)
            + (4 # 121) * max0(2 - s V_III_reorder_sb)
            - (0 # 1) * max0(36 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                                  - 18 * s V_III_reorder_sb)
            - (0 # 1) * max0(575 - s V_III_reorder_l)
            - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
            + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
            + (0 # 1) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | 58 => hints
     [(*-5.14563e-05 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-36
                                                                    + 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (-36
                                                                    + 18 * s V_III_reorder_sb));
      (*-5.14563e-05 0*) F_binom_monotonic 2 (F_max0_ge_0 (-36
                                                           + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0));
      (*0 6.12101e-05*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_III_reorder_sb) (0))) (F_max0_ge_0 (-2
                                                                    + s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-4.8595e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (-1
                                                                    + 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.000874711 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*0 0.000874711*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - s V_III_reorder_sb) (0))) (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-6.13511e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - s V_III_reorder_sb) (0))) (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (575
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*-5.15022e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - s V_III_reorder_sb) (0))) (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-5.15022e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                    - s V_III_reorder_sb)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-3.43572e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_max0_ge_0 (575
                                                                    - 18 * s V_III_reorder_sb))) (F_binom_monotonic 1 (F_max0_ge_0 (35
                                                                    - s V_III_reorder_l)) (F_check_ge (0) (0)));
      (*0 3.05969e-06*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (575
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (575
                                                                    - 18 * s V_III_reorder_sb) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (36
                                                                    - 18 * s V_III_reorder_sb)) (F_check_ge (0) (0)));
      (*-0.0331296 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                           - s V_III_reorder_sb)) (F_check_ge (2
                                                                    - s V_III_reorder_sb) (0))]
     ((579 # 1) - (6 # 173) * s V_III_reorder_sb
      - (0 # 1) * s V_III_reorder_sb * max0(-1 + 18 * s V_III_reorder_sb)
      + (0 # 1) * s V_III_reorder_sb * max0(36 - 18 * s V_III_reorder_sb)
      - (0 # 1) * s V_III_reorder_sb * max0(18 * s V_III_reorder_sb)
      + (1 # 60) * s V_III_reorder_sb^2 + s V_III_reorder_z
      + (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb)
      - (0 # 1) * max0(-1 + 18 * s V_III_reorder_sb) * max0(36
                                                            - 18 * s V_III_reorder_sb)
      + (4 # 121) * max0(2 - s V_III_reorder_sb)
      - (0 # 1) * max0(36 - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(36 - 18 * s V_III_reorder_sb) * max0(575
                                                            - 18 * s V_III_reorder_sb)
      - (0 # 1) * max0(575 - s V_III_reorder_l)
      - (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)
      + (0 # 1) * max0(575 - 18 * s V_III_reorder_sb)^2
      + (0 # 1) * max0(18 * s V_III_reorder_sb) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_reorder =>
    [mkPA Q (fun n z s => ai_III_reorder n s /\ annot0_III_reorder n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_reorder (proc_start P_III_reorder) s1 (proc_end P_III_reorder) s2 ->
    (s2 V_III_reorder_z <= (579 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_reorder.
Qed.
