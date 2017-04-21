Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zreadline_from.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zreadline_from_z := 1%positive.
Notation V_zreadline_from__tmp := 2%positive.
Notation V_zreadline_from__tmp1 := 3%positive.
Notation V_zreadline_from_ch := 4%positive.
Notation V_zreadline_from_ch1 := 5%positive.
Notation V_zreadline_from_count := 6%positive.
Notation V_zreadline_from_pcount_dref := 7%positive.
Notation V_zreadline_from_pin_eol_dref := 8%positive.
Notation V_zreadline_from_pcount := 9%positive.
Notation V_zreadline_from_pin_eol := 10%positive.
Notation V_zreadline_from_ptr := 11%positive.
Notation V_zreadline_from_s := 12%positive.
Notation V_zreadline_from_size := 13%positive.
Definition Pedges_zreadline_from: list (edge proc) :=
  (EA 1 (AAssign V_zreadline_from_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_count) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_zreadline_from__tmp1)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_zreadline_from__tmp1 (Some (EVar V_zreadline_from_size))) 6)::
  (EA 6 (AAssign V_zreadline_from_count
  (Some (EVar V_zreadline_from_pcount_dref))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_pin_eol_dref) s) <>
  (eval (ENum (0)) s))%Z)) 49)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_pin_eol_dref) s) = (eval (ENum (0))
  s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::(EA 12 AWeaken 13)::
  (EA 13 ANone 15)::(EA 13 ANone 14)::(EA 14 ANone 16)::(EA 15 ANone 16)::
  (EA 16 (AAssign V_zreadline_from_ch1 None) 17)::(EA 17 AWeaken 18)::
  (EA 18 (AGuard (fun s => ((eval (EVar V_zreadline_from_ch1) s) <
  (eval (ENum (0)) s))%Z)) 44)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_ch1) s) >= (eval (ENum (0))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 ANone 31)::(EA 20 ANone 25)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_zreadline_from_pcount_dref
  (Some (EVar V_zreadline_from_count))) 22)::(EA 22 (AAssign
  V_zreadline_from__tmp (Some (ENum (0)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 75)::(EA 25 (AAssign V_zreadline_from_pcount_dref
  (Some (EVar V_zreadline_from_count))) 26)::(EA 26 (AAssign
  V_zreadline_from_pin_eol_dref (Some (ENum (1)))) 27)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V_zreadline_from_z (Some (EAdd (ENum (1))
  (EVar V_zreadline_from_z)))) 30)::(EA 30 AWeaken 9)::(EA 31 AWeaken 32)::
  (EA 32 (AGuard (fun s => ((eval (EVar V_zreadline_from_count) s) >=
  (eval (EVar V_zreadline_from__tmp1) s))%Z)) 39)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_count) s) <
  (eval (EVar V_zreadline_from__tmp1) s))%Z)) 33)::(EA 33 AWeaken 34)::
  (EA 34 (AAssign V_zreadline_from_count
  (Some (EAdd (EVar V_zreadline_from_count) (ENum (1))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_zreadline_from_z
  (Some (EAdd (ENum (1)) (EVar V_zreadline_from_z)))) 38)::
  (EA 38 AWeaken 13)::(EA 39 AWeaken 40)::(EA 40 (AAssign
  V_zreadline_from_pcount_dref (Some (EVar V_zreadline_from_count))) 41)::
  (EA 41 (AAssign V_zreadline_from__tmp (Some (ENum (1)))) 42)::
  (EA 42 ANone 43)::(EA 43 AWeaken 75)::(EA 44 AWeaken 45)::(EA 45 (AAssign
  V_zreadline_from_pcount_dref (Some (EVar V_zreadline_from_count))) 46)::
  (EA 46 (AAssign V_zreadline_from__tmp
  (Some (EVar V_zreadline_from_ch1))) 47)::(EA 47 ANone 48)::
  (EA 48 AWeaken 75)::(EA 49 AWeaken 50)::(EA 50 (AAssign V_zreadline_from_ch
  None) 51)::(EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_ch) s) = (eval (ENum (-1))
  s))%Z)) 70)::(EA 52 (AGuard (fun s => ((eval (EVar V_zreadline_from_ch)
  s) <> (eval (ENum (-1)) s))%Z)) 53)::(EA 53 AWeaken 54)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_ch) s) < (eval (ENum (0))
  s))%Z)) 66)::(EA 54 (AGuard (fun s => ((eval (EVar V_zreadline_from_ch)
  s) >= (eval (ENum (0)) s))%Z)) 55)::(EA 55 AWeaken 56)::(EA 56 (AGuard
  (fun s => ((eval (EVar V_zreadline_from_ch) s) <> (eval (ENum (10))
  s))%Z)) 58)::(EA 56 (AGuard (fun s => ((eval (EVar V_zreadline_from_ch)
  s) = (eval (ENum (10)) s))%Z)) 57)::(EA 57 AWeaken 60)::
  (EA 58 AWeaken 59)::(EA 59 ANone 60)::(EA 60 ANone 61)::(EA 61 ANone 62)::
  (EA 62 (AAssign V_zreadline_from_pin_eol_dref (Some (ENum (0)))) 63)::
  (EA 63 (AAssign V_zreadline_from__tmp (Some (ENum (0)))) 64)::
  (EA 64 ANone 65)::(EA 65 AWeaken 75)::(EA 66 AWeaken 67)::(EA 67 (AAssign
  V_zreadline_from__tmp (Some (EVar V_zreadline_from_ch))) 68)::
  (EA 68 ANone 69)::(EA 69 AWeaken 75)::(EA 70 AWeaken 71)::(EA 71 (AAssign
  V_zreadline_from_pin_eol_dref (Some (ENum (0)))) 72)::(EA 72 (AAssign
  V_zreadline_from__tmp (Some (ENum (0)))) 73)::(EA 73 ANone 74)::
  (EA 74 AWeaken 75)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zreadline_from => Pedges_zreadline_from
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zreadline_from => 75
     end)%positive;
  var_global := var_global
}.

Definition ai_zreadline_from (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 3 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_count <= 0)%Z
   | 4 => (-1 * s V_zreadline_from_count <= 0 /\ 1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from__tmp1 <= 0)%Z
   | 5 => (-1 * s V_zreadline_from__tmp1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_count <= 0)%Z
   | 6 => (-1 * s V_zreadline_from_count <= 0 /\ 1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 7 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_z <= 0)%Z
   | 8 => (1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 9 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 10 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 11 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 12 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 13 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 14 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 15 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 16 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 17 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 18 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 19 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0)%Z
   | 20 => (-1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 21 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0)%Z
   | 22 => (-1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 23 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from__tmp <= 0 /\ -1 * s V_zreadline_from__tmp <= 0)%Z
   | 24 => (-1 * s V_zreadline_from__tmp <= 0 /\ 1 * s V_zreadline_from__tmp <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 25 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0)%Z
   | 26 => (-1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 27 => (-1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref + -1 <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref + 1 <= 0)%Z
   | 28 => (-1 * s V_zreadline_from_pin_eol_dref + 1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref + -1 <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 29 => (-1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref + -1 <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref + 1 <= 0)%Z
   | 30 => (-1 * s V_zreadline_from_pin_eol_dref + 1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref + -1 <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z + 1 <= 0)%Z
   | 31 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0)%Z
   | 32 => (-1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 33 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from__tmp1+ 1 * s V_zreadline_from_count + 1 <= 0)%Z
   | 34 => (-1 * s V_zreadline_from__tmp1+ 1 * s V_zreadline_from_count + 1 <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 35 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from__tmp1+ 1 * s V_zreadline_from_count <= 0)%Z
   | 36 => (-1 * s V_zreadline_from__tmp1+ 1 * s V_zreadline_from_count <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 37 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from__tmp1+ 1 * s V_zreadline_from_count <= 0)%Z
   | 38 => (-1 * s V_zreadline_from__tmp1+ 1 * s V_zreadline_from_count <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z + 1 <= 0)%Z
   | 39 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_count <= 0)%Z
   | 40 => (1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_count <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 41 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_count <= 0 /\ 1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_pcount_dref <= 0)%Z
   | 42 => (1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_pcount_dref <= 0 /\ 1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_count <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from__tmp + -1 <= 0 /\ -1 * s V_zreadline_from__tmp + 1 <= 0)%Z
   | 43 => (-1 * s V_zreadline_from__tmp + 1 <= 0 /\ 1 * s V_zreadline_from__tmp + -1 <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch1 <= 0 /\ 1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_count <= 0 /\ 1 * s V_zreadline_from__tmp1+ -1 * s V_zreadline_from_pcount_dref <= 0)%Z
   | 44 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch1 + 1 <= 0)%Z
   | 45 => (1 * s V_zreadline_from_ch1 + 1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 46 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch1 + 1 <= 0)%Z
   | 47 => (1 * s V_zreadline_from_ch1 + 1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from__tmp + 1 <= 0)%Z
   | 48 => (1 * s V_zreadline_from__tmp + 1 <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch1 + 1 <= 0)%Z
   | 49 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 50 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 51 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 52 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 53 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 54 => (-1 * s V_zreadline_from_z <= 0)%Z
   | 55 => (-1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch <= 0)%Z
   | 56 => (-1 * s V_zreadline_from_ch <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 57 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch + -10 <= 0 /\ -1 * s V_zreadline_from_ch + 10 <= 0)%Z
   | 58 => (-1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch <= 0)%Z
   | 59 => (-1 * s V_zreadline_from_ch <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 60 => (-1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch <= 0)%Z
   | 61 => (-1 * s V_zreadline_from_ch <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 62 => (-1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch <= 0)%Z
   | 63 => (-1 * s V_zreadline_from_ch <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 64 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ -1 * s V_zreadline_from_ch <= 0 /\ 1 * s V_zreadline_from__tmp <= 0 /\ -1 * s V_zreadline_from__tmp <= 0)%Z
   | 65 => (-1 * s V_zreadline_from__tmp <= 0 /\ 1 * s V_zreadline_from__tmp <= 0 /\ -1 * s V_zreadline_from_ch <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 66 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0)%Z
   | 67 => (1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 68 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ 1 * s V_zreadline_from__tmp + 1 <= 0)%Z
   | 69 => (1 * s V_zreadline_from__tmp + 1 <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 70 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_ch + -1 <= 0)%Z
   | 71 => (-1 * s V_zreadline_from_ch + -1 <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | 72 => (-1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_ch + -1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 73 => (-1 * s V_zreadline_from_pin_eol_dref <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_ch + -1 <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from__tmp <= 0 /\ -1 * s V_zreadline_from__tmp <= 0)%Z
   | 74 => (-1 * s V_zreadline_from__tmp <= 0 /\ 1 * s V_zreadline_from__tmp <= 0 /\ -1 * s V_zreadline_from_z <= 0 /\ 1 * s V_zreadline_from_ch + 1 <= 0 /\ -1 * s V_zreadline_from_ch + -1 <= 0 /\ 1 * s V_zreadline_from_pin_eol_dref <= 0 /\ -1 * s V_zreadline_from_pin_eol_dref <= 0)%Z
   | 75 => (1 * s V_zreadline_from__tmp + -1 <= 0 /\ -1 * s V_zreadline_from_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zreadline_from (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(-s V_zreadline_from_pcount_dref + s V_zreadline_from_size) <= z)%Q
   | 2 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(-s V_zreadline_from_pcount_dref + s V_zreadline_from_size) <= z)%Q
   | 3 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(-s V_zreadline_from_pcount_dref + s V_zreadline_from_size) <= z)%Q
   | 4 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(-s V_zreadline_from_pcount_dref + s V_zreadline_from_size) <= z)%Q
   | 5 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(-s V_zreadline_from_pcount_dref + s V_zreadline_from_size) <= z)%Q
   | 6 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_pcount_dref) <= z)%Q
   | 7 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 8 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 9 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
           + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 10 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zreadline_from_z) (0))) (F_max0_ge_0 (s V_zreadline_from_z))]
     (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 11 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 12 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 13 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 14 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 15 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 16 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 17 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 18 => (max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zreadline_from_z)) (F_check_ge (s V_zreadline_from_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zreadline_from_pin_eol_dref) (0))) (F_max0_ge_0 (-
                                                                    s V_zreadline_from_pin_eol_dref));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   - s V_zreadline_from_pin_eol_dref)) (F_check_ge (1
                                                                    - s V_zreadline_from_pin_eol_dref) (0))]
     (max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
      + max0(s V_zreadline_from_z) <= z)%Q
   | 20 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 21 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 22 => ((1 # 1) + s V_zreadline_from_z
            - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-1 + s V_zreadline_from__tmp1
                   - s V_zreadline_from_pcount_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 23 => ((1 # 1) + s V_zreadline_from_z
            - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-1 + s V_zreadline_from__tmp1
                   - s V_zreadline_from_pcount_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                             - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zreadline_from_pin_eol_dref) (0))) (F_max0_ge_0 (s V_zreadline_from_pin_eol_dref));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_zreadline_from_pin_eol_dref) (0))) (F_max0_ge_0 (1
                                                                    - s V_zreadline_from_pin_eol_dref));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_zreadline_from__tmp1
                                                 - s V_zreadline_from_pcount_dref)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_zreadline_from_z
      - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
      + max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_pcount_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 25 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 26 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 27 => ((1 # 1) + s V_zreadline_from_z
            + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 28 => ((1 # 1) + s V_zreadline_from_z
            + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 29 => ((1 # 1) + s V_zreadline_from_z
            + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 30 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 31 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 32 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_zreadline_from__tmp1
                                       - s V_zreadline_from_count) (1)]
     ((1 # 1) + s V_zreadline_from_z
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 34 => ((2 # 1) + s V_zreadline_from_z
            + max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 35 => ((2 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 36 => ((2 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 37 => ((2 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zreadline_from_z) (0))) (F_max0_ge_0 (s V_zreadline_from_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zreadline_from_pin_eol_dref) (0))) (F_max0_ge_0 (s V_zreadline_from_pin_eol_dref));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_zreadline_from_pin_eol_dref) (0))) (F_max0_ge_0 (1
                                                                    - s V_zreadline_from_pin_eol_dref))]
     ((1 # 1) + s V_zreadline_from_z
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 39 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 40 => ((1 # 1) + s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 41 => ((1 # 1) + s V_zreadline_from_z
            - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-1 + s V_zreadline_from__tmp1
                   - s V_zreadline_from_pcount_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 42 => ((1 # 1) + s V_zreadline_from_z
            - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-1 + s V_zreadline_from__tmp1
                   - s V_zreadline_from_pcount_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 43 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                             - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_zreadline_from__tmp1
                                                 - s V_zreadline_from_pcount_dref)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_zreadline_from_z
      - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
      + max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_pcount_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 44 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zreadline_from_pin_eol_dref) (0))) (F_max0_ge_0 (-
                                                                    s V_zreadline_from_pin_eol_dref))]
     (max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
      + max0(s V_zreadline_from_z) <= z)%Q
   | 45 => (s V_zreadline_from_pin_eol_dref
            + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 46 => (s V_zreadline_from_pin_eol_dref
            - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-1 + s V_zreadline_from__tmp1
                   - s V_zreadline_from_pcount_dref)
            + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 47 => (s V_zreadline_from_pin_eol_dref
            - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-1 + s V_zreadline_from__tmp1
                   - s V_zreadline_from_pcount_dref)
            + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
            + max0(-s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from_z) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                             - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zreadline_from_z)) (F_check_ge (s V_zreadline_from_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   - s V_zreadline_from_pin_eol_dref)) (F_check_ge (1
                                                                    - s V_zreadline_from_pin_eol_dref) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_zreadline_from__tmp1
                                                 - s V_zreadline_from_pcount_dref)) (F_check_ge (0) (0))]
     (s V_zreadline_from_pin_eol_dref
      - max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_count)
      + max0(-1 + s V_zreadline_from__tmp1 - s V_zreadline_from_pcount_dref)
      + max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count)
      + max0(-s V_zreadline_from_pin_eol_dref) + max0(s V_zreadline_from_z) <= z)%Q
   | 49 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 50 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 51 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 52 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 53 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 54 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 55 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 56 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 57 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                            - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_zreadline_from__tmp1
                                                 - s V_zreadline_from_count)) (F_check_ge (0) (0))]
     (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 58 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                             - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_zreadline_from__tmp1
                                                 - s V_zreadline_from_count)) (F_check_ge (0) (0))]
     (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 59 => (s V_zreadline_from_z <= z)%Q
   | 60 => (s V_zreadline_from_z <= z)%Q
   | 61 => (s V_zreadline_from_z <= z)%Q
   | 62 => (s V_zreadline_from_z <= z)%Q
   | 63 => (s V_zreadline_from_z <= z)%Q
   | 64 => (s V_zreadline_from_z <= z)%Q
   | 65 => (s V_zreadline_from_z <= z)%Q
   | 66 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 67 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 68 => (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 69 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                             - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zreadline_from__tmp1
                            - s V_zreadline_from_count);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0))]
     (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zreadline_from_pin_eol_dref)) (F_check_ge (0) (0))]
     (s V_zreadline_from_z + max0(1 - s V_zreadline_from_pin_eol_dref)
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 71 => (s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 72 => (s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 73 => (s V_zreadline_from_z
            + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 74 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zreadline_from__tmp1
                                             - s V_zreadline_from_count) (-1
                                                                    + s V_zreadline_from__tmp1
                                                                    - s V_zreadline_from_count));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zreadline_from__tmp1
                            - s V_zreadline_from_count)]
     (s V_zreadline_from_z
      + max0(s V_zreadline_from__tmp1 - s V_zreadline_from_count) <= z)%Q
   | 75 => (s V_zreadline_from_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zreadline_from =>
    [mkPA Q (fun n z s => ai_zreadline_from n s /\ annot0_zreadline_from n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zreadline_from (proc_start P_zreadline_from) s1 (proc_end P_zreadline_from) s2 ->
    (s2 V_zreadline_from_z <= max0(1 - s1 V_zreadline_from_pin_eol_dref)
                              + max0(-s1 V_zreadline_from_pcount_dref
                                     + s1 V_zreadline_from_size))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zreadline_from.
Qed.
