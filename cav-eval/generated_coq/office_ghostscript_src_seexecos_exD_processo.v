Require Import pasta.Pasta.

Inductive proc: Type :=
  P_s_exD_process.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_s_exD_process_z := 1%positive.
Notation V_s_exD_process__tmp := 2%positive.
Notation V_s_exD_process__tmp1 := 3%positive.
Notation V_s_exD_process_count := 4%positive.
Notation V_s_exD_process_i := 5%positive.
Notation V_s_exD_process_rcount := 6%positive.
Notation V_s_exD_process_skip := 7%positive.
Notation V_s_exD_process_status := 8%positive.
Notation V_s_exD_process_wcount := 9%positive.
Notation V_s_exD_process_last := 10%positive.
Notation V_s_exD_process_pr := 11%positive.
Notation V_s_exD_process_pw := 12%positive.
Notation V_s_exD_process_st := 13%positive.
Definition Pedges_s_exD_process: list (edge proc) :=
  (EA 1 (AAssign V_s_exD_process_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_s_exD_process__tmp1 (Some (EVar V_s_exD_process_last))) 3)::
  (EA 3 (AAssign V_s_exD_process_skip None) 4)::(EA 4 (AAssign
  V_s_exD_process_rcount None) 5)::(EA 5 (AAssign V_s_exD_process_wcount
  None) 6)::(EA 6 (AAssign V_s_exD_process_status (Some (ENum (0)))) 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_wcount) s) <
  (eval (EVar V_s_exD_process_rcount) s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_wcount) s) >=
  (eval (EVar V_s_exD_process_rcount) s))%Z)) 9)::(EA 9 AWeaken 10)::
  (EA 10 ANone 14)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_s_exD_process_status (Some (ENum (1)))) 13)::(EA 13 ANone 14)::
  (EA 14 (AAssign V_s_exD_process_count None) 15)::(EA 15 AWeaken 16)::
  (EA 16 ANone 18)::(EA 16 ANone 17)::(EA 17 AWeaken 36)::
  (EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_rcount) s) < (eval (ENum (8))
  s))%Z)) 72)::(EA 19 (AGuard (fun s => ((eval (EVar V_s_exD_process_rcount)
  s) >= (eval (ENum (8)) s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AAssign
  V_s_exD_process_i (Some (ENum (1)))) 22)::(EA 22 ANone 23)::
  (EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_i) s) <= (eval (ENum (8))
  s))%Z)) 26)::(EA 24 (AGuard (fun s => ((eval (EVar V_s_exD_process_i) s) >
  (eval (ENum (8)) s))%Z)) 25)::(EA 25 AWeaken 34)::(EA 26 AWeaken 27)::
  (EA 27 ANone 66)::(EA 27 ANone 28)::(EA 28 AWeaken 29)::(EA 29 ANone 66)::
  (EA 29 ANone 30)::(EA 30 AWeaken 31)::(EA 31 ANone 32)::(EA 31 ANone 33)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 ANone 35)::(EA 35 AWeaken 36)::
  (EA 36 ANone 41)::(EA 36 ANone 37)::(EA 37 (AAssign V_s_exD_process_status
  None) 38)::(EA 38 (AAssign V_s_exD_process_count None) 39)::
  (EA 39 ANone 40)::(EA 40 AWeaken 52)::(EA 41 AWeaken 42)::
  (EA 42 ANone 44)::(EA 42 ANone 43)::(EA 43 AWeaken 48)::(EA 44 (AAssign
  V_s_exD_process_count None) 45)::(EA 45 (AAssign V_s_exD_process_status
  (Some (ENum (0)))) 46)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::
  (EA 48 ANone 49)::(EA 48 ANone 50)::(EA 49 ANone 50)::(EA 50 ANone 51)::
  (EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_skip) s) >=
  (eval (EVar V_s_exD_process_count) s))%Z)) 54)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_skip) s) <
  (eval (EVar V_s_exD_process_count) s))%Z)) 53)::(EA 53 AWeaken 57)::
  (EA 54 AWeaken 55)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_s_exD_process_skip) s) <> (eval (ENum (0))
  s))%Z)) 59)::(EA 55 (AGuard (fun s => ((eval (EVar V_s_exD_process_skip)
  s) = (eval (ENum (0)) s))%Z)) 56)::(EA 56 AWeaken 57)::(EA 57 (AAssign
  V_s_exD_process_count (Some (ESub (EVar V_s_exD_process_count)
  (EVar V_s_exD_process_skip)))) 58)::(EA 58 ANone 63)::(EA 59 AWeaken 60)::
  (EA 60 (AAssign V_s_exD_process_count (Some (ENum (0)))) 61)::
  (EA 61 (AAssign V_s_exD_process_status (Some (ENum (0)))) 62)::
  (EA 62 ANone 63)::(EA 63 (AAssign V_s_exD_process__tmp
  (Some (EVar V_s_exD_process_status))) 64)::(EA 64 ANone 65)::
  (EA 65 AWeaken 76)::(EA 66 ANone 67)::(EA 67 (AAssign V_s_exD_process_i
  (Some (EAdd (EVar V_s_exD_process_i) (ENum (1))))) 68)::(EA 68 ANone 69)::
  (EA 69 ANone 70)::(EA 70 (AAssign V_s_exD_process_z (Some (EAdd (ENum (1))
  (EVar V_s_exD_process_z)))) 71)::(EA 71 AWeaken 24)::(EA 72 AWeaken 73)::
  (EA 73 (AAssign V_s_exD_process__tmp (Some (ENum (0)))) 74)::
  (EA 74 ANone 75)::(EA 75 AWeaken 76)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_s_exD_process => Pedges_s_exD_process
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_s_exD_process => 76
     end)%positive;
  var_global := var_global
}.

Definition ai_s_exD_process (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 3 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0)%Z
   | 4 => (1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 5 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0)%Z
   | 6 => (1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 7 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 8 => (-1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 9 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_rcount+ -1 * s V_s_exD_process_wcount <= 0)%Z
   | 10 => (1 * s V_s_exD_process_rcount+ -1 * s V_s_exD_process_wcount <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 11 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount+ 1 * s V_s_exD_process_wcount + 1 <= 0)%Z
   | 12 => (-1 * s V_s_exD_process_rcount+ 1 * s V_s_exD_process_wcount + 1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 13 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_rcount+ 1 * s V_s_exD_process_wcount + 1 <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status + 1 <= 0)%Z
   | 14 => (-1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 15 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 16 => (-1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 17 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 18 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 19 => (-1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 20 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0)%Z
   | 21 => (-1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 22 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ 1 * s V_s_exD_process_i + -1 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0)%Z
   | 23 => (-1 * s V_s_exD_process_i + 1 <= 0 /\ 1 * s V_s_exD_process_i + -1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 24 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_i + -9 <= 0)%Z
   | 25 => (1 * s V_s_exD_process_i + -9 <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 9 <= 0)%Z
   | 26 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -8 <= 0)%Z
   | 27 => (1 * s V_s_exD_process_i + -8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 28 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -8 <= 0)%Z
   | 29 => (1 * s V_s_exD_process_i + -8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 30 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -8 <= 0)%Z
   | 31 => (1 * s V_s_exD_process_i + -8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 32 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -8 <= 0)%Z
   | 33 => (1 * s V_s_exD_process_i + -8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 34 => (1 * s V_s_exD_process_i + -9 <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 35 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_i + -9 <= 0)%Z
   | 36 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 37 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 38 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 39 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 40 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 41 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 42 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 43 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 44 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 45 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 46 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 47 => (-1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 48 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 49 => (-1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 50 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 51 => (-1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 52 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 53 => (-1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_count+ 1 * s V_s_exD_process_skip + 1 <= 0)%Z
   | 54 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_count+ -1 * s V_s_exD_process_skip <= 0)%Z
   | 55 => (1 * s V_s_exD_process_count+ -1 * s V_s_exD_process_skip <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 56 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_count+ -1 * s V_s_exD_process_skip <= 0 /\ 1 * s V_s_exD_process_skip <= 0 /\ -1 * s V_s_exD_process_skip <= 0)%Z
   | 57 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 58 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 59 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_count+ -1 * s V_s_exD_process_skip <= 0)%Z
   | 60 => (1 * s V_s_exD_process_count+ -1 * s V_s_exD_process_skip <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 61 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_count <= 0 /\ -1 * s V_s_exD_process_count <= 0)%Z
   | 62 => (-1 * s V_s_exD_process_count <= 0 /\ 1 * s V_s_exD_process_count <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_status <= 0)%Z
   | 63 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 64 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 65 => (-1 * s V_s_exD_process_z <= 0)%Z
   | 66 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -8 <= 0)%Z
   | 67 => (1 * s V_s_exD_process_i + -8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_i + 1 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 68 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -9 <= 0 /\ -1 * s V_s_exD_process_i + 2 <= 0)%Z
   | 69 => (-1 * s V_s_exD_process_i + 2 <= 0 /\ 1 * s V_s_exD_process_i + -9 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0)%Z
   | 70 => (1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_i + -9 <= 0 /\ -1 * s V_s_exD_process_i + 2 <= 0)%Z
   | 71 => (-1 * s V_s_exD_process_i + 2 <= 0 /\ 1 * s V_s_exD_process_i + -9 <= 0 /\ -1 * s V_s_exD_process_rcount + 8 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_z + 1 <= 0)%Z
   | 72 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_rcount + -7 <= 0)%Z
   | 73 => (1 * s V_s_exD_process_rcount + -7 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 74 => (-1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_rcount + -7 <= 0 /\ 1 * s V_s_exD_process__tmp <= 0 /\ -1 * s V_s_exD_process__tmp <= 0)%Z
   | 75 => (-1 * s V_s_exD_process__tmp <= 0 /\ 1 * s V_s_exD_process__tmp <= 0 /\ 1 * s V_s_exD_process_rcount + -7 <= 0 /\ -1 * s V_s_exD_process_status <= 0 /\ 1 * s V_s_exD_process_status + -1 <= 0 /\ 1 * s V_s_exD_process_z <= 0 /\ -1 * s V_s_exD_process_z <= 0)%Z
   | 76 => (-1 * s V_s_exD_process_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_s_exD_process (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 3 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 4 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 5 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 6 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 7 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 8 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 9 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 10 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 11 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 12 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 13 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 14 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 15 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 16 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 17 => hints
     [(*-8 0*) F_one]
     ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 18 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 19 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 20 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 21 => ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 22 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 23 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 24 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 25 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_s_exD_process_i) (1)]
     (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 27 => ((1 # 1) + s V_s_exD_process_z + max0(8 - s V_s_exD_process_i) <= z)%Q
   | 28 => ((1 # 1) + s V_s_exD_process_z + max0(8 - s V_s_exD_process_i) <= z)%Q
   | 29 => ((1 # 1) + s V_s_exD_process_z + max0(8 - s V_s_exD_process_i) <= z)%Q
   | 30 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                              - s V_s_exD_process_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_s_exD_process_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8 - s V_s_exD_process_i)) (F_check_ge (8
                                                                    - s V_s_exD_process_i) (0))]
     ((1 # 1) + s V_s_exD_process_z + max0(8 - s V_s_exD_process_i) <= z)%Q
   | 31 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 32 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 33 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 34 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9 - s V_s_exD_process_i)) (F_check_ge (0) (0))]
     (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 36 => (s V_s_exD_process_z <= z)%Q
   | 37 => (s V_s_exD_process_z <= z)%Q
   | 38 => (s V_s_exD_process_z <= z)%Q
   | 39 => (s V_s_exD_process_z <= z)%Q
   | 40 => (s V_s_exD_process_z <= z)%Q
   | 41 => (s V_s_exD_process_z <= z)%Q
   | 42 => (s V_s_exD_process_z <= z)%Q
   | 43 => (s V_s_exD_process_z <= z)%Q
   | 44 => (s V_s_exD_process_z <= z)%Q
   | 45 => (s V_s_exD_process_z <= z)%Q
   | 46 => (s V_s_exD_process_z <= z)%Q
   | 47 => (s V_s_exD_process_z <= z)%Q
   | 48 => (s V_s_exD_process_z <= z)%Q
   | 49 => (s V_s_exD_process_z <= z)%Q
   | 50 => (s V_s_exD_process_z <= z)%Q
   | 51 => (s V_s_exD_process_z <= z)%Q
   | 52 => (s V_s_exD_process_z <= z)%Q
   | 53 => (s V_s_exD_process_z <= z)%Q
   | 54 => (s V_s_exD_process_z <= z)%Q
   | 55 => (s V_s_exD_process_z <= z)%Q
   | 56 => (s V_s_exD_process_z <= z)%Q
   | 57 => (s V_s_exD_process_z <= z)%Q
   | 58 => (s V_s_exD_process_z <= z)%Q
   | 59 => (s V_s_exD_process_z <= z)%Q
   | 60 => (s V_s_exD_process_z <= z)%Q
   | 61 => (s V_s_exD_process_z <= z)%Q
   | 62 => (s V_s_exD_process_z <= z)%Q
   | 63 => (s V_s_exD_process_z <= z)%Q
   | 64 => (s V_s_exD_process_z <= z)%Q
   | 65 => (s V_s_exD_process_z <= z)%Q
   | 66 => ((1 # 1) + s V_s_exD_process_z + max0(8 - s V_s_exD_process_i) <= z)%Q
   | 67 => ((1 # 1) + s V_s_exD_process_z + max0(8 - s V_s_exD_process_i) <= z)%Q
   | 68 => ((1 # 1) + s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 69 => ((1 # 1) + s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 70 => ((1 # 1) + s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 71 => (s V_s_exD_process_z + max0(9 - s V_s_exD_process_i) <= z)%Q
   | 72 => hints
     [(*-8 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_s_exD_process_status) (0))) (F_max0_ge_0 (1
                                                                    - s V_s_exD_process_status))]
     ((8 # 1) + s V_s_exD_process_z <= z)%Q
   | 73 => ((8 # 1) * s V_s_exD_process_status + s V_s_exD_process_z
            + (8 # 1) * max0(1 - s V_s_exD_process_status) <= z)%Q
   | 74 => ((8 # 1) * s V_s_exD_process_status + s V_s_exD_process_z
            + (8 # 1) * max0(1 - s V_s_exD_process_status) <= z)%Q
   | 75 => hints
     [(*-8 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_s_exD_process_status)) (F_check_ge (0) (0));
      (*-8 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_s_exD_process_status) (0))) (F_max0_ge_0 (s V_s_exD_process_status));
      (*-8 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_s_exD_process_status)) (F_check_ge (0) (0))]
     ((8 # 1) * s V_s_exD_process_status + s V_s_exD_process_z
      + (8 # 1) * max0(1 - s V_s_exD_process_status) <= z)%Q
   | 76 => (s V_s_exD_process_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_s_exD_process =>
    [mkPA Q (fun n z s => ai_s_exD_process n s /\ annot0_s_exD_process n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_s_exD_process (proc_start P_s_exD_process) s1 (proc_end P_s_exD_process) s2 ->
    (s2 V_s_exD_process_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_s_exD_process.
Qed.
