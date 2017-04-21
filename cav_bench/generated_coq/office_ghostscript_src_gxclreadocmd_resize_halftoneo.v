Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_resize_halftone.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_resize_halftone_z := 1%positive.
Notation V_cmd_resize_halftone__tmp := 2%positive.
Notation V_cmd_resize_halftone__tmp1 := 3%positive.
Notation V_cmd_resize_halftone_i := 4%positive.
Notation V_cmd_resize_halftone_pdht_dref_off136 := 5%positive.
Notation V_cmd_resize_halftone_mem := 6%positive.
Notation V_cmd_resize_halftone_num_comp := 7%positive.
Notation V_cmd_resize_halftone_pdht := 8%positive.
Definition Pedges_cmd_resize_halftone: list (edge proc) :=
  (EA 1 (AAssign V_cmd_resize_halftone_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign V_cmd_resize_halftone__tmp
  (Some (EVar V_cmd_resize_halftone_num_comp))) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) <>
  (eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s))%Z)) 10)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) =
  (eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s))%Z)) 9)::
  (EA 9 AWeaken 45)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) <
  (eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s))%Z)) 25)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) >=
  (eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s))%Z)) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s) =
  (eval (ENum (0)) s))%Z)) 17)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone_pdht_dref_off136) s) <>
  (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::
  (EA 16 AWeaken 20)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 20)::(EA 20 ANone 22)::(EA 20 ANone 21)::(EA 21 ANone 43)::
  (EA 22 (AAssign V_cmd_resize_halftone__tmp1 (Some (ENum (-25)))) 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 48)::(EA 25 AWeaken 26)::(EA 26 (AAssign
  V_cmd_resize_halftone_i
  (Some (EVar V_cmd_resize_halftone_pdht_dref_off136))) 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_cmd_resize_halftone_i
  (Some (EAdd (EVar V_cmd_resize_halftone_i) (ENum (-1))))) 29)::
  (EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone_i) s) >
  (eval (EVar V_cmd_resize_halftone__tmp) s))%Z)) 49)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone_i) s) <=
  (eval (EVar V_cmd_resize_halftone__tmp) s))%Z)) 31)::(EA 31 AWeaken 32)::
  (EA 32 (AGuard (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) =
  (eval (ENum (0)) s))%Z)) 40)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_cmd_resize_halftone__tmp) s) <> (eval (ENum (0))
  s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 ANone 36)::(EA 34 ANone 35)::
  (EA 35 ANone 42)::(EA 36 (AAssign V_cmd_resize_halftone_pdht_dref_off136
  (Some (EVar V_cmd_resize_halftone__tmp))) 37)::(EA 37 (AAssign
  V_cmd_resize_halftone__tmp1 (Some (ENum (-25)))) 38)::(EA 38 ANone 39)::
  (EA 39 AWeaken 48)::(EA 40 AWeaken 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_cmd_resize_halftone_pdht_dref_off136
  (Some (EVar V_cmd_resize_halftone__tmp))) 44)::(EA 44 ANone 45)::
  (EA 45 (AAssign V_cmd_resize_halftone__tmp1 (Some (ENum (0)))) 46)::
  (EA 46 ANone 47)::(EA 47 AWeaken 48)::(EA 49 AWeaken 50)::
  (EA 50 ANone 51)::(EA 50 ANone 52)::(EA 51 ANone 52)::(EA 52 ANone 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_cmd_resize_halftone_z
  (Some (EAdd (ENum (1)) (EVar V_cmd_resize_halftone_z)))) 28)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_resize_halftone => Pedges_cmd_resize_halftone
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_resize_halftone => 48
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_resize_halftone (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0)%Z
   | 3 => (-1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 4 => (-1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 5 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp <= 0)%Z
   | 6 => (-1 * s V_cmd_resize_halftone__tmp <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 7 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 8 => (-1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 9 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 10 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 11 => (-1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 12 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 13 => (-1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 14 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 15 => (-1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 16 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 17 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 18 => (1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 19 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 20 => (-1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 21 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 22 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 23 => (-1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 + 25 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp1 + -25 <= 0)%Z
   | 24 => (-1 * s V_cmd_resize_halftone__tmp1 + -25 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 + 25 <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 25 => (-1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 26 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 27 => (-1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 28 => (-1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0)%Z
   | 29 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 30 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 31 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 32 => (-1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 33 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 34 => (-1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 35 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 36 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 37 => (-1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_i+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 38 => (1 * s V_cmd_resize_halftone_i+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_i+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 + 25 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp1 + -25 <= 0)%Z
   | 39 => (-1 * s V_cmd_resize_halftone__tmp1 + -25 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 + 25 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_i+ 1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone_i+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 40 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp <= 0 /\ -1 * s V_cmd_resize_halftone__tmp <= 0)%Z
   | 41 => (-1 * s V_cmd_resize_halftone__tmp <= 0 /\ 1 * s V_cmd_resize_halftone__tmp <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 42 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i <= 0 /\ -1 * s V_cmd_resize_halftone__tmp+ 1 * s V_cmd_resize_halftone_i <= 0)%Z
   | 43 => (-1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0)%Z
   | 44 => (-1 * s V_cmd_resize_halftone_z <= 0)%Z
   | 45 => (-1 * s V_cmd_resize_halftone_z <= 0)%Z
   | 46 => (-1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 <= 0 /\ -1 * s V_cmd_resize_halftone__tmp1 <= 0)%Z
   | 47 => (-1 * s V_cmd_resize_halftone__tmp1 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0)%Z
   | 48 => (-1 * s V_cmd_resize_halftone__tmp1 + -25 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp1 <= 0)%Z
   | 49 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0)%Z
   | 50 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 51 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0)%Z
   | 52 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | 53 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0)%Z
   | 54 => (1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_i + 1 <= 0 /\ -1 * s V_cmd_resize_halftone_z <= 0 /\ -1 * s V_cmd_resize_halftone_pdht_dref_off136 <= 0 /\ 1 * s V_cmd_resize_halftone__tmp+ -1 * s V_cmd_resize_halftone_pdht_dref_off136 + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_resize_halftone (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 - s V_cmd_resize_halftone_num_comp
                + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 2 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone_num_comp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 3 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone_num_comp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 4 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone_num_comp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 5 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone_num_comp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 6 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone_num_comp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 7 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone__tmp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 8 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone__tmp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 9 => (s V_cmd_resize_halftone_z
           + max0(-1 - s V_cmd_resize_halftone__tmp
                  + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 10 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 11 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_resize_halftone_z) (0))) (F_max0_ge_0 (s V_cmd_resize_halftone_z))]
     (s V_cmd_resize_halftone_z
      + max0(-1 - s V_cmd_resize_halftone__tmp
             + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 13 => (max0(-1 - s V_cmd_resize_halftone__tmp
                 + s V_cmd_resize_halftone_pdht_dref_off136)
            + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 14 => (max0(-1 - s V_cmd_resize_halftone__tmp
                 + s V_cmd_resize_halftone_pdht_dref_off136)
            + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 15 => (max0(-1 - s V_cmd_resize_halftone__tmp
                 + s V_cmd_resize_halftone_pdht_dref_off136)
            + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_resize_halftone_z)) (F_check_ge (s V_cmd_resize_halftone_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 - s V_cmd_resize_halftone__tmp
                                                 + s V_cmd_resize_halftone_pdht_dref_off136)) (F_check_ge (0) (0))]
     (max0(-1 - s V_cmd_resize_halftone__tmp
           + s V_cmd_resize_halftone_pdht_dref_off136)
      + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 17 => (max0(-1 - s V_cmd_resize_halftone__tmp
                 + s V_cmd_resize_halftone_pdht_dref_off136)
            + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 18 => (max0(-1 - s V_cmd_resize_halftone__tmp
                 + s V_cmd_resize_halftone_pdht_dref_off136)
            + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_resize_halftone_z)) (F_check_ge (s V_cmd_resize_halftone_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 - s V_cmd_resize_halftone__tmp
                                                 + s V_cmd_resize_halftone_pdht_dref_off136)) (F_check_ge (0) (0))]
     (max0(-1 - s V_cmd_resize_halftone__tmp
           + s V_cmd_resize_halftone_pdht_dref_off136)
      + max0(s V_cmd_resize_halftone_z) <= z)%Q
   | 20 => (s V_cmd_resize_halftone_z <= z)%Q
   | 21 => (s V_cmd_resize_halftone_z <= z)%Q
   | 22 => (s V_cmd_resize_halftone_z <= z)%Q
   | 23 => (s V_cmd_resize_halftone_z <= z)%Q
   | 24 => (s V_cmd_resize_halftone_z <= z)%Q
   | 25 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 26 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 27 => (s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 28 => (s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 29 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 30 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cmd_resize_halftone__tmp
                                                               + s V_cmd_resize_halftone_i) (0))) (F_max0_ge_0 (-
                                                                    s V_cmd_resize_halftone__tmp
                                                                    + s V_cmd_resize_halftone_i));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  - s V_cmd_resize_halftone__tmp
                                                  + s V_cmd_resize_halftone_pdht_dref_off136)) (F_check_ge (-1
                                                                    - s V_cmd_resize_halftone__tmp
                                                                    + s V_cmd_resize_halftone_pdht_dref_off136) (0))]
     ((1 # 1) + s V_cmd_resize_halftone_i
      - s V_cmd_resize_halftone_pdht_dref_off136 + s V_cmd_resize_halftone_z
      + max0(-1 - s V_cmd_resize_halftone__tmp
             + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 32 => (s V_cmd_resize_halftone_z
            + max0(-s V_cmd_resize_halftone__tmp + s V_cmd_resize_halftone_i) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cmd_resize_halftone__tmp
                                             + s V_cmd_resize_halftone_i) (-1
                                                                    - s V_cmd_resize_halftone__tmp
                                                                    + s V_cmd_resize_halftone_i));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cmd_resize_halftone__tmp
                            + s V_cmd_resize_halftone_i)]
     (s V_cmd_resize_halftone_z
      + max0(-s V_cmd_resize_halftone__tmp + s V_cmd_resize_halftone_i) <= z)%Q
   | 34 => (s V_cmd_resize_halftone_z <= z)%Q
   | 35 => (s V_cmd_resize_halftone_z <= z)%Q
   | 36 => (s V_cmd_resize_halftone_z <= z)%Q
   | 37 => (s V_cmd_resize_halftone_z <= z)%Q
   | 38 => (s V_cmd_resize_halftone_z <= z)%Q
   | 39 => (s V_cmd_resize_halftone_z <= z)%Q
   | 40 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cmd_resize_halftone__tmp
                                             + s V_cmd_resize_halftone_i) (-1
                                                                    - s V_cmd_resize_halftone__tmp
                                                                    + s V_cmd_resize_halftone_i));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cmd_resize_halftone__tmp
                            + s V_cmd_resize_halftone_i)]
     (s V_cmd_resize_halftone_z
      + max0(-s V_cmd_resize_halftone__tmp + s V_cmd_resize_halftone_i) <= z)%Q
   | 41 => (s V_cmd_resize_halftone_z <= z)%Q
   | 42 => (s V_cmd_resize_halftone_z <= z)%Q
   | 43 => (s V_cmd_resize_halftone_z <= z)%Q
   | 44 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 45 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 46 => (s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 - s V_cmd_resize_halftone__tmp
                                                 + s V_cmd_resize_halftone_pdht_dref_off136)) (F_check_ge (0) (0))]
     (s V_cmd_resize_halftone_z
      + max0(-1 - s V_cmd_resize_halftone__tmp
             + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 48 => (s V_cmd_resize_halftone_z <= z)%Q
   | 49 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 50 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 51 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 52 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 53 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | 54 => ((1 # 1) + s V_cmd_resize_halftone_i
            - s V_cmd_resize_halftone_pdht_dref_off136
            + s V_cmd_resize_halftone_z
            + max0(-1 - s V_cmd_resize_halftone__tmp
                   + s V_cmd_resize_halftone_pdht_dref_off136) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_resize_halftone =>
    [mkPA Q (fun n z s => ai_cmd_resize_halftone n s /\ annot0_cmd_resize_halftone n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_resize_halftone (proc_start P_cmd_resize_halftone) s1 (proc_end P_cmd_resize_halftone) s2 ->
    (s2 V_cmd_resize_halftone_z <= max0(-1
                                        - s1 V_cmd_resize_halftone_num_comp
                                        + s1 V_cmd_resize_halftone_pdht_dref_off136))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_resize_halftone.
Qed.
