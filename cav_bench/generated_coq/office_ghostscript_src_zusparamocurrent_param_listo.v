Require Import pasta.Pasta.

Inductive proc: Type :=
  P_current_param_list.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_current_param_list_z := 1%positive.
Notation V_current_param_list__tmp := 2%positive.
Notation V_current_param_list_code := 3%positive.
Notation V_current_param_list_code3 := 4%positive.
Notation V_current_param_list_code6 := 5%positive.
Notation V_current_param_list_i := 6%positive.
Notation V_current_param_list_pset_dref_off24 := 7%positive.
Notation V_current_param_list_pset_dref_off40 := 8%positive.
Notation V_current_param_list_pset_dref_off8 := 9%positive.
Notation V_current_param_list_op := 10%positive.
Notation V_current_param_list_pset := 11%positive.
Notation V_current_param_list_psref := 12%positive.
Definition Pedges_current_param_list: list (edge proc) :=
  (EA 1 (AAssign V_current_param_list_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_current_param_list_pset_dref_off8) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_current_param_list_pset_dref_off40) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_current_param_list_pset_dref_off24) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_current_param_list_i) s) >= (eval (ENum (0))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_current_param_list_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_current_param_list_i) s) <
  (eval (EVar V_current_param_list_pset_dref_off8) s))%Z)) 63)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_current_param_list_i) s) >=
  (eval (EVar V_current_param_list_pset_dref_off8) s))%Z)) 11)::
  (EA 11 AWeaken 12)::(EA 12 (AAssign V_current_param_list_i
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_current_param_list_i) s) <
  (eval (EVar V_current_param_list_pset_dref_off24) s))%Z)) 44)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_current_param_list_i) s) >=
  (eval (EVar V_current_param_list_pset_dref_off24) s))%Z)) 16)::
  (EA 16 AWeaken 17)::(EA 17 (AAssign V_current_param_list_i
  (Some (ENum (0)))) 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_current_param_list_i) s) <
  (eval (EVar V_current_param_list_pset_dref_off40) s))%Z)) 25)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_current_param_list_i) s) >=
  (eval (EVar V_current_param_list_pset_dref_off40) s))%Z)) 21)::
  (EA 21 AWeaken 22)::(EA 22 (AAssign V_current_param_list__tmp
  (Some (ENum (0)))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 82)::
  (EA 25 AWeaken 26)::(EA 26 ANone 29)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 ANone 34)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_current_param_list_code6 None) 30)::(EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_current_param_list_code6) s) < (eval (ENum (0))
  s))%Z)) 40)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_current_param_list_code6) s) >= (eval (ENum (0))
  s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 ANone 35)::
  (EA 35 (AAssign V_current_param_list_i
  (Some (EAdd (EVar V_current_param_list_i) (ENum (1))))) 36)::
  (EA 36 ANone 37)::(EA 37 ANone 38)::(EA 38 (AAssign V_current_param_list_z
  (Some (EAdd (ENum (1)) (EVar V_current_param_list_z)))) 39)::
  (EA 39 AWeaken 20)::(EA 40 AWeaken 41)::(EA 41 (AAssign
  V_current_param_list__tmp (Some (EVar V_current_param_list_code6))) 42)::
  (EA 42 ANone 43)::(EA 43 AWeaken 82)::(EA 44 AWeaken 45)::
  (EA 45 ANone 48)::(EA 45 ANone 46)::(EA 46 AWeaken 47)::(EA 47 ANone 53)::
  (EA 47 ANone 48)::(EA 48 (AAssign V_current_param_list_code3 None) 49)::
  (EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_current_param_list_code3) s) < (eval (ENum (0))
  s))%Z)) 59)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_current_param_list_code3) s) >= (eval (ENum (0))
  s))%Z)) 51)::(EA 51 AWeaken 52)::(EA 52 ANone 53)::(EA 53 ANone 54)::
  (EA 54 (AAssign V_current_param_list_i
  (Some (EAdd (EVar V_current_param_list_i) (ENum (1))))) 55)::
  (EA 55 ANone 56)::(EA 56 ANone 57)::(EA 57 (AAssign V_current_param_list_z
  (Some (EAdd (ENum (1)) (EVar V_current_param_list_z)))) 58)::
  (EA 58 AWeaken 15)::(EA 59 AWeaken 60)::(EA 60 (AAssign
  V_current_param_list__tmp (Some (EVar V_current_param_list_code3))) 61)::
  (EA 61 ANone 62)::(EA 62 AWeaken 82)::(EA 63 AWeaken 64)::
  (EA 64 ANone 67)::(EA 64 ANone 65)::(EA 65 AWeaken 66)::(EA 66 ANone 72)::
  (EA 66 ANone 67)::(EA 67 (AAssign V_current_param_list_code None) 68)::
  (EA 68 AWeaken 69)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_current_param_list_code) s) < (eval (ENum (0))
  s))%Z)) 78)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_current_param_list_code) s) >= (eval (ENum (0))
  s))%Z)) 70)::(EA 70 AWeaken 71)::(EA 71 ANone 72)::(EA 72 ANone 73)::
  (EA 73 (AAssign V_current_param_list_i
  (Some (EAdd (EVar V_current_param_list_i) (ENum (1))))) 74)::
  (EA 74 ANone 75)::(EA 75 ANone 76)::(EA 76 (AAssign V_current_param_list_z
  (Some (EAdd (ENum (1)) (EVar V_current_param_list_z)))) 77)::
  (EA 77 AWeaken 10)::(EA 78 AWeaken 79)::(EA 79 (AAssign
  V_current_param_list__tmp (Some (EVar V_current_param_list_code))) 80)::
  (EA 80 ANone 81)::(EA 81 AWeaken 82)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_current_param_list => Pedges_current_param_list
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_current_param_list => 82
     end)%positive;
  var_global := var_global
}.

Definition ai_current_param_list (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_z <= 0)%Z
   | 3 => (-1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off8 <= 0)%Z
   | 4 => (-1 * s V_current_param_list_pset_dref_off8 <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 5 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 6 => (-1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 7 => (-1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 8 => (-1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 9 => (-1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 10 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0)%Z
   | 11 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off8 <= 0)%Z
   | 12 => (-1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0)%Z
   | 13 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 14 => (-1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 15 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 16 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 17 => (-1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 18 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 19 => (-1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 20 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 21 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 22 => (-1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 23 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list__tmp <= 0 /\ -1 * s V_current_param_list__tmp <= 0)%Z
   | 24 => (-1 * s V_current_param_list__tmp <= 0 /\ 1 * s V_current_param_list__tmp <= 0 /\ -1 * s V_current_param_list_i+ 1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 25 => (-1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 26 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 27 => (-1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 28 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 29 => (-1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 30 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 31 => (-1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 32 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_code6 <= 0)%Z
   | 33 => (-1 * s V_current_param_list_code6 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 34 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0)%Z
   | 35 => (-1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 36 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 37 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0)%Z
   | 38 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 39 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ -1 * s V_current_param_list_z + 1 <= 0)%Z
   | 40 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_code6 + 1 <= 0)%Z
   | 41 => (1 * s V_current_param_list_code6 + 1 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 42 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ 1 * s V_current_param_list_code6 + 1 <= 0 /\ 1 * s V_current_param_list__tmp + 1 <= 0)%Z
   | 43 => (1 * s V_current_param_list__tmp + 1 <= 0 /\ 1 * s V_current_param_list_code6 + 1 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off40 + 1 <= 0)%Z
   | 44 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 45 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 46 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 47 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 48 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 49 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 50 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 51 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_code3 <= 0)%Z
   | 52 => (-1 * s V_current_param_list_code3 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 53 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 54 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 55 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 56 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z <= 0)%Z
   | 57 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0)%Z
   | 58 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_z + 1 <= 0)%Z
   | 59 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_code3 + 1 <= 0)%Z
   | 60 => (1 * s V_current_param_list_code3 + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 61 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_code3 + 1 <= 0 /\ 1 * s V_current_param_list__tmp + 1 <= 0)%Z
   | 62 => (1 * s V_current_param_list__tmp + 1 <= 0 /\ 1 * s V_current_param_list_code3 + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off24 + 1 <= 0)%Z
   | 63 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 64 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 65 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 66 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 67 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 68 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 69 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 70 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_code <= 0)%Z
   | 71 => (-1 * s V_current_param_list_code <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 72 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | 73 => (-1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 74 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0)%Z
   | 75 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_z <= 0)%Z
   | 76 => (-1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0)%Z
   | 77 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 <= 0 /\ -1 * s V_current_param_list_i + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_z + 1 <= 0)%Z
   | 78 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_code + 1 <= 0)%Z
   | 79 => (1 * s V_current_param_list_code + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 80 => (1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ 1 * s V_current_param_list_code + 1 <= 0 /\ 1 * s V_current_param_list__tmp + 1 <= 0)%Z
   | 81 => (1 * s V_current_param_list__tmp + 1 <= 0 /\ 1 * s V_current_param_list_code + 1 <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0 /\ -1 * s V_current_param_list_pset_dref_off24 <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ 1 * s V_current_param_list_i+ -1 * s V_current_param_list_pset_dref_off8 + 1 <= 0)%Z
   | 82 => (1 * s V_current_param_list__tmp <= 0 /\ -1 * s V_current_param_list_z <= 0 /\ -1 * s V_current_param_list_i <= 0 /\ -1 * s V_current_param_list_pset_dref_off40 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_current_param_list (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 2 => (s V_current_param_list_z
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 3 => (s V_current_param_list_z
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 4 => (s V_current_param_list_z
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 5 => (s V_current_param_list_z
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 6 => (s V_current_param_list_z
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 7 => (s V_current_param_list_z
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40)
           + max0(s V_current_param_list_pset_dref_off8) <= z)%Q
   | 8 => (s V_current_param_list_z
           + max0(-s V_current_param_list_i
                  + s V_current_param_list_pset_dref_off8)
           + max0(s V_current_param_list_pset_dref_off24)
           + max0(s V_current_param_list_pset_dref_off40) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_current_param_list_pset_dref_off40)) (F_check_ge (s V_current_param_list_pset_dref_off40) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_current_param_list_pset_dref_off24)) (F_check_ge (s V_current_param_list_pset_dref_off24) (0))]
     (s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off8)
      + max0(s V_current_param_list_pset_dref_off24)
      + max0(s V_current_param_list_pset_dref_off40) <= z)%Q
   | 10 => (s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_current_param_list_i
                                             + s V_current_param_list_pset_dref_off8) (-1
                                                                    - s V_current_param_list_i
                                                                    + s V_current_param_list_pset_dref_off8));
      (*-1 0*) F_max0_ge_0 (-1 - s V_current_param_list_i
                            + s V_current_param_list_pset_dref_off8)]
     (s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 12 => (s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 13 => (s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24)
            - max0(s V_current_param_list_pset_dref_off24) <= z)%Q
   | 14 => (s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24)
            - max0(s V_current_param_list_pset_dref_off24) <= z)%Q
   | 15 => (s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24)
            - max0(s V_current_param_list_pset_dref_off24) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_current_param_list_i
                                             + s V_current_param_list_pset_dref_off24) (-1
                                                                    - s V_current_param_list_i
                                                                    + s V_current_param_list_pset_dref_off24));
      (*-1 0*) F_max0_ge_0 (-1 - s V_current_param_list_i
                            + s V_current_param_list_pset_dref_off24);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_current_param_list_pset_dref_off24) (0))) (F_max0_ge_0 (s V_current_param_list_pset_dref_off24))]
     (s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off24)
      - max0(s V_current_param_list_pset_dref_off24) <= z)%Q
   | 17 => (s V_current_param_list_pset_dref_off40 + s V_current_param_list_z <= z)%Q
   | 18 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 19 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 20 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 21 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_current_param_list_i
                                                              + s V_current_param_list_pset_dref_off40) (0))) (F_max0_ge_0 (-
                                                                    s V_current_param_list_i
                                                                    + s V_current_param_list_pset_dref_off40))]
     (-s V_current_param_list_i + s V_current_param_list_pset_dref_off40
      + s V_current_param_list_z <= z)%Q
   | 22 => (s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off40) <= z)%Q
   | 23 => (s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off40) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_current_param_list_i
                                             + s V_current_param_list_pset_dref_off40) (-1
                                                                    - s V_current_param_list_i
                                                                    + s V_current_param_list_pset_dref_off40));
      (*-1 0*) F_max0_ge_0 (-1 - s V_current_param_list_i
                            + s V_current_param_list_pset_dref_off40)]
     (s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off40) <= z)%Q
   | 25 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 26 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 27 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 28 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 29 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 30 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 31 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 32 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 33 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 34 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 35 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 36 => ((1 # 1) - s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 37 => ((1 # 1) - s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 38 => ((1 # 1) - s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 39 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 40 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 41 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 42 => (-s V_current_param_list_i
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 43 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (-s V_current_param_list_i
                                       + s V_current_param_list_pset_dref_off40) (1);
      (*-1 0*) F_max0_ge_0 (-1 - s V_current_param_list_i
                            + s V_current_param_list_pset_dref_off40);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_current_param_list_i
                                                               + s V_current_param_list_pset_dref_off40) (0))) (F_max0_ge_0 (-
                                                                    s V_current_param_list_i
                                                                    + s V_current_param_list_pset_dref_off40))]
     (-s V_current_param_list_i + s V_current_param_list_pset_dref_off40
      + s V_current_param_list_z <= z)%Q
   | 44 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_current_param_list_i
                                      + s V_current_param_list_pset_dref_off24) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_current_param_list_pset_dref_off24) (0))) (F_max0_ge_0 (s V_current_param_list_pset_dref_off24))]
     (s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off24)
      - max0(s V_current_param_list_pset_dref_off24) <= z)%Q
   | 45 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 46 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 47 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 48 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 49 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 50 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 51 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 52 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 53 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 54 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 55 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 56 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 57 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 58 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_current_param_list_pset_dref_off24)) (F_check_ge (s V_current_param_list_pset_dref_off24) (0))]
     (s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 59 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_current_param_list_i
                                                + s V_current_param_list_pset_dref_off24)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_current_param_list_pset_dref_off40
      + s V_current_param_list_z
      + max0(-1 - s V_current_param_list_i
             + s V_current_param_list_pset_dref_off24) <= z)%Q
   | 60 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 61 => ((1 # 1) + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 62 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (s V_current_param_list_pset_dref_off40);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_current_param_list_pset_dref_off40) (0))) (F_max0_ge_0 (s V_current_param_list_pset_dref_off40))]
     ((1 # 1) + s V_current_param_list_pset_dref_off40
      + s V_current_param_list_z <= z)%Q
   | 63 => hints
     [(*-8e-12 1*) F_max0_pre_decrement 1 (-s V_current_param_list_i
                                           + s V_current_param_list_pset_dref_off8) (1)]
     (s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 64 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 65 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 66 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 67 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 68 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 69 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 70 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 71 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 72 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 73 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-1 - s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 74 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 75 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 76 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z
            + max0(-s V_current_param_list_i
                   + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 77 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_current_param_list_z)) (F_check_ge (s V_current_param_list_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_current_param_list_z) (0))) (F_max0_ge_0 (s V_current_param_list_z))]
     (s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-s V_current_param_list_i
             + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 78 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_current_param_list_i
                                                + s V_current_param_list_pset_dref_off8)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z
      + max0(-1 - s V_current_param_list_i
             + s V_current_param_list_pset_dref_off8) <= z)%Q
   | 79 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 80 => ((1 # 1) + s V_current_param_list_pset_dref_off24
            + s V_current_param_list_pset_dref_off40
            + s V_current_param_list_z <= z)%Q
   | 81 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (s V_current_param_list_pset_dref_off24);
      (*-1 0*) F_max0_ge_0 (s V_current_param_list_pset_dref_off40);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_current_param_list_pset_dref_off40) (0))) (F_max0_ge_0 (s V_current_param_list_pset_dref_off40));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_current_param_list_pset_dref_off24) (0))) (F_max0_ge_0 (s V_current_param_list_pset_dref_off24))]
     ((1 # 1) + s V_current_param_list_pset_dref_off24
      + s V_current_param_list_pset_dref_off40 + s V_current_param_list_z <= z)%Q
   | 82 => (s V_current_param_list_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_current_param_list =>
    [mkPA Q (fun n z s => ai_current_param_list n s /\ annot0_current_param_list n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_current_param_list (proc_start P_current_param_list) s1 (proc_end P_current_param_list) s2 ->
    (s2 V_current_param_list_z <= max0(s1 V_current_param_list_pset_dref_off24)
                                  + max0(s1 V_current_param_list_pset_dref_off40)
                                  + max0(s1 V_current_param_list_pset_dref_off8))%Q.
Proof.
  prove_bound ipa admissible_ipa P_current_param_list.
Qed.
