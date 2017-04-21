Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_put_ht_order.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_put_ht_order_z := 1%positive.
Notation V_cmd_put_ht_order__tmp := 2%positive.
Notation V_cmd_put_ht_order__tmp1 := 3%positive.
Notation V_cmd_put_ht_order__tmp2 := 4%positive.
Notation V_cmd_put_ht_order_i := 5%positive.
Notation V_cmd_put_ht_order_len := 6%positive.
Notation V_cmd_put_ht_order_n := 7%positive.
Notation V_cmd_put_ht_order_porder_dref_off56 := 8%positive.
Notation V_cmd_put_ht_order_porder_dref_off60 := 9%positive.
Notation V_cmd_put_ht_order_cldev := 10%positive.
Notation V_cmd_put_ht_order_cname := 11%positive.
Notation V_cmd_put_ht_order_component := 12%positive.
Notation V_cmd_put_ht_order_porder := 13%positive.
Definition Pedges_cmd_put_ht_order: list (edge proc) :=
  (EA 1 (AAssign V_cmd_put_ht_order_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order_porder_dref_off60) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order_porder_dref_off56) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order_n) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order_i)
  s) >= (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_cmd_put_ht_order__tmp2 (Some (EVar V_cmd_put_ht_order_cname))) 8)::
  (EA 8 (AAssign V_cmd_put_ht_order__tmp
  (Some (EVar V_cmd_put_ht_order_component))) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 12)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order__tmp) s) < (eval (ENum (0))
  s))%Z)) 11)::(EA 11 AWeaken 14)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::
  (EA 14 (AAssign V_cmd_put_ht_order_len None) 15)::(EA 15 ANone 16)::
  (EA 16 AWeaken 17)::(EA 17 ANone 74)::(EA 17 ANone 18)::(EA 18 ANone 19)::
  (EA 19 (AAssign V_cmd_put_ht_order_i (Some (ENum (0)))) 20)::
  (EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order_i) s) <
  (eval (EVar V_cmd_put_ht_order_porder_dref_off56) s))%Z)) 53)::
  (EA 22 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order_i) s) >=
  (eval (EVar V_cmd_put_ht_order_porder_dref_off56) s))%Z)) 23)::
  (EA 23 AWeaken 24)::(EA 24 (AAssign V_cmd_put_ht_order_i
  (Some (ENum (0)))) 25)::(EA 25 ANone 26)::(EA 26 AWeaken 27)::
  (EA 27 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order_i) s) <
  (eval (EVar V_cmd_put_ht_order_porder_dref_off60) s))%Z)) 32)::
  (EA 27 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order_i) s) >=
  (eval (EVar V_cmd_put_ht_order_porder_dref_off60) s))%Z)) 28)::
  (EA 28 AWeaken 29)::(EA 29 (AAssign V_cmd_put_ht_order__tmp1
  (Some (ENum (0)))) 30)::(EA 30 ANone 31)::(EA 31 AWeaken 77)::
  (EA 32 AWeaken 33)::(EA 33 (AAssign V_cmd_put_ht_order_n
  (Some (ESub (EVar V_cmd_put_ht_order_porder_dref_off60)
  (EVar V_cmd_put_ht_order_i)))) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order_n) s) > (eval (ENum (99))
  s))%Z)) 37)::(EA 35 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order_n)
  s) <= (eval (ENum (99)) s))%Z)) 36)::(EA 36 AWeaken 40)::
  (EA 37 AWeaken 38)::(EA 38 (AAssign V_cmd_put_ht_order_n
  (Some (ENum (99)))) 39)::(EA 39 ANone 40)::(EA 40 ANone 41)::
  (EA 41 AWeaken 42)::(EA 42 ANone 50)::(EA 42 ANone 43)::(EA 43 ANone 44)::
  (EA 44 ANone 45)::(EA 45 (AAssign V_cmd_put_ht_order_i
  (Some (EAdd (EVar V_cmd_put_ht_order_i)
  (EVar V_cmd_put_ht_order_n)))) 46)::(EA 46 ANone 47)::(EA 47 ANone 48)::
  (EA 48 (AAssign V_cmd_put_ht_order_z (Some (EAdd (ENum (1))
  (EVar V_cmd_put_ht_order_z)))) 49)::(EA 49 AWeaken 27)::(EA 50 (AAssign
  V_cmd_put_ht_order__tmp1 None) 51)::(EA 51 ANone 52)::(EA 52 AWeaken 77)::
  (EA 53 AWeaken 54)::(EA 54 (AAssign V_cmd_put_ht_order_n
  (Some (ESub (EVar V_cmd_put_ht_order_porder_dref_off56)
  (EVar V_cmd_put_ht_order_i)))) 55)::(EA 55 AWeaken 56)::(EA 56 (AGuard
  (fun s => ((eval (EVar V_cmd_put_ht_order_n) s) > (eval (ENum (199))
  s))%Z)) 58)::(EA 56 (AGuard (fun s => ((eval (EVar V_cmd_put_ht_order_n)
  s) <= (eval (ENum (199)) s))%Z)) 57)::(EA 57 AWeaken 61)::
  (EA 58 AWeaken 59)::(EA 59 (AAssign V_cmd_put_ht_order_n
  (Some (ENum (199)))) 60)::(EA 60 ANone 61)::(EA 61 ANone 62)::
  (EA 62 AWeaken 63)::(EA 63 ANone 71)::(EA 63 ANone 64)::(EA 64 ANone 65)::
  (EA 65 ANone 66)::(EA 66 (AAssign V_cmd_put_ht_order_i
  (Some (EAdd (EVar V_cmd_put_ht_order_i)
  (EVar V_cmd_put_ht_order_n)))) 67)::(EA 67 ANone 68)::(EA 68 ANone 69)::
  (EA 69 (AAssign V_cmd_put_ht_order_z (Some (EAdd (ENum (1))
  (EVar V_cmd_put_ht_order_z)))) 70)::(EA 70 AWeaken 22)::(EA 71 (AAssign
  V_cmd_put_ht_order__tmp1 None) 72)::(EA 72 ANone 73)::(EA 73 AWeaken 77)::
  (EA 74 (AAssign V_cmd_put_ht_order__tmp1 None) 75)::(EA 75 ANone 76)::
  (EA 76 AWeaken 77)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_put_ht_order => Pedges_cmd_put_ht_order
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_put_ht_order => 77
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_put_ht_order (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0)%Z
   | 3 => (-1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 4 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 5 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 6 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 7 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 8 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 9 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 10 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 11 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ 1 * s V_cmd_put_ht_order__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order__tmp <= 0)%Z
   | 13 => (-1 * s V_cmd_put_ht_order__tmp <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 14 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 15 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 16 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 17 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 18 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 19 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 20 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ 1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 21 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ 1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 22 => (-1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 23 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_i+ 1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 24 => (-1 * s V_cmd_put_ht_order_i+ 1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 25 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 26 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ 1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 27 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 28 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_i+ 1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 29 => (-1 * s V_cmd_put_ht_order_i+ 1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 30 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_i+ 1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order__tmp1 <= 0 /\ -1 * s V_cmd_put_ht_order__tmp1 <= 0)%Z
   | 31 => (-1 * s V_cmd_put_ht_order__tmp1 <= 0 /\ 1 * s V_cmd_put_ht_order__tmp1 <= 0 /\ -1 * s V_cmd_put_ht_order_i+ 1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 32 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0)%Z
   | 33 => (1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 34 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 35 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 36 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0)%Z
   | 37 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 100 <= 0)%Z
   | 38 => (-1 * s V_cmd_put_ht_order_n + 100 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 39 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 99 <= 0)%Z
   | 40 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 41 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 42 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 43 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 44 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 45 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 46 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0)%Z
   | 47 => (1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 48 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0)%Z
   | 49 => (1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z + 1 <= 0)%Z
   | 50 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 51 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | 52 => (-1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off60 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -99 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 53 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0)%Z
   | 54 => (1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 55 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 56 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 57 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0)%Z
   | 58 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 200 <= 0)%Z
   | 59 => (-1 * s V_cmd_put_ht_order_n + 200 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n + 2 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 60 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 199 <= 0)%Z
   | 61 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 62 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 63 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 64 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 65 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 66 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 67 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0)%Z
   | 68 => (1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 69 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0)%Z
   | 70 => (1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_n+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z + 1 <= 0)%Z
   | 71 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 72 => (-1 * s V_cmd_put_ht_order_n + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0)%Z
   | 73 => (-1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_i+ -1 * s V_cmd_put_ht_order_porder_dref_off56 + 1 <= 0 /\ 1 * s V_cmd_put_ht_order_n + -199 <= 0 /\ -1 * s V_cmd_put_ht_order_n + 1 <= 0)%Z
   | 74 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 75 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_i <= 0)%Z
   | 76 => (-1 * s V_cmd_put_ht_order_i <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ 1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_n <= 0)%Z
   | 77 => (-1 * s V_cmd_put_ht_order_n <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off60 <= 0 /\ -1 * s V_cmd_put_ht_order_z <= 0 /\ -1 * s V_cmd_put_ht_order_porder_dref_off56 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_put_ht_order (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 2 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 3 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 4 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 5 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 6 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 7 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 8 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 9 => (s V_cmd_put_ht_order_z
           + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
           + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 10 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 11 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 12 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 13 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 14 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 15 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 16 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 17 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 18 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 19 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 20 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 21 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 22 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (198 - s V_cmd_put_ht_order_i
                                             + s V_cmd_put_ht_order_porder_dref_off56) (198
                                                                    - s V_cmd_put_ht_order_i
                                                                    - s V_cmd_put_ht_order_n
                                                                    + s V_cmd_put_ht_order_porder_dref_off56));
      (*-1 0*) F_max0_ge_0 (198 - s V_cmd_put_ht_order_i
                            - s V_cmd_put_ht_order_n
                            + s V_cmd_put_ht_order_porder_dref_off56)]
     (s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
      + max0(198 - s V_cmd_put_ht_order_i
             + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 24 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 25 => (s V_cmd_put_ht_order_z
            + max0(98 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 26 => (s V_cmd_put_ht_order_z
            + max0(98 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 27 => (s V_cmd_put_ht_order_z
            + max0(98 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 28 => (s V_cmd_put_ht_order_z
            + max0(98 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 29 => (s V_cmd_put_ht_order_z
            + max0(98 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 30 => (s V_cmd_put_ht_order_z
            + max0(98 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (98 - s V_cmd_put_ht_order_i
                                             + s V_cmd_put_ht_order_porder_dref_off60) (98
                                                                    - s V_cmd_put_ht_order_i
                                                                    - s V_cmd_put_ht_order_n
                                                                    + s V_cmd_put_ht_order_porder_dref_off60));
      (*-1 0*) F_max0_ge_0 (98 - s V_cmd_put_ht_order_i
                            - s V_cmd_put_ht_order_n
                            + s V_cmd_put_ht_order_porder_dref_off60)]
     (s V_cmd_put_ht_order_z
      + max0(98 - s V_cmd_put_ht_order_i
             + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 32 => hints
     [(*0 0.496222*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (198
                                                                    - 
                                                                    s V_cmd_put_ht_order_i
                                                                    + 
                                                                    s V_cmd_put_ht_order_porder_dref_off60) (0))) (F_max0_ge_0 (198
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off60));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (98 - s V_cmd_put_ht_order_i
                                                  + s V_cmd_put_ht_order_porder_dref_off60)) (F_check_ge (98
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off60) (0))]
     (s V_cmd_put_ht_order_z
      + max0(98 - s V_cmd_put_ht_order_i
             + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 33 => (-(32 # 127) - (65 # 129) * s V_cmd_put_ht_order_i
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 34 => (-(32 # 127) - (65 # 129) * s V_cmd_put_ht_order_i
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 35 => (-(32 # 127) - (65 # 129) * s V_cmd_put_ht_order_i
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 36 => hints
     [(*0 0.00126577*) F_binom_monotonic 1 (F_max0_ge_arg (199
                                                           - s V_cmd_put_ht_order_n)) (F_check_ge (199
                                                                    - s V_cmd_put_ht_order_n) (0))]
     (-(32 # 127) - (65 # 129) * s V_cmd_put_ht_order_i
      + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
      + s V_cmd_put_ht_order_z
      + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                          + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 37 => (-(32 # 127) - (65 # 129) * s V_cmd_put_ht_order_i
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 38 => (-(32 # 127) - (65 # 129) * s V_cmd_put_ht_order_i
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 39 => (-(65 # 129) * s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60)
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 40 => (-(65 # 129) * s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
            + s V_cmd_put_ht_order_z
            + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                                + s V_cmd_put_ht_order_porder_dref_off60)
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 41 => hints
     [(*-0.496222 0*) F_binom_monotonic 1 (F_max0_ge_arg (198
                                                          - s V_cmd_put_ht_order_i
                                                          + s V_cmd_put_ht_order_porder_dref_off60)) (F_check_ge (198
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off60) (0))]
     (-(65 # 129) * s V_cmd_put_ht_order_i - (0 # 1) * s V_cmd_put_ht_order_n
      + (65 # 129) * s V_cmd_put_ht_order_porder_dref_off60
      + s V_cmd_put_ht_order_z
      + (64 # 129) * max0(198 - s V_cmd_put_ht_order_i
                          + s V_cmd_put_ht_order_porder_dref_off60)
      - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 42 => ((12478 # 127) - s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 43 => ((12478 # 127) - s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 44 => ((12478 # 127) - s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 45 => ((12478 # 127) - s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 46 => ((12478 # 127) - s V_cmd_put_ht_order_i
            + (1 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 47 => ((12478 # 127) - s V_cmd_put_ht_order_i
            + (1 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 48 => ((12478 # 127) - s V_cmd_put_ht_order_i
            + (1 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 49 => hints
     [(*-0.00126577 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (199
                                                                    - s V_cmd_put_ht_order_n) (0))) (F_max0_ge_0 (199
                                                                    - s V_cmd_put_ht_order_n));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (98
                                                               - s V_cmd_put_ht_order_i
                                                               + s V_cmd_put_ht_order_porder_dref_off60) (0))) (F_max0_ge_0 (98
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off60));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_cmd_put_ht_order_n)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_cmd_put_ht_order_n) (0))) (F_max0_ge_0 (-1
                                                                    + s V_cmd_put_ht_order_n))]
     ((12351 # 127) - s V_cmd_put_ht_order_i
      + (1 # 1) * s V_cmd_put_ht_order_n
      + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
      - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 50 => ((12478 # 127) - s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 51 => ((12478 # 127) - s V_cmd_put_ht_order_i
            - (0 # 1) * s V_cmd_put_ht_order_n
            + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
            - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 52 => hints
     [(*-98.5 0*) F_one;
      (*-0.5 0*) F_max0_monotonic (F_check_ge (-1
                                               - 2 * s V_cmd_put_ht_order_i
                                               + 2 * s V_cmd_put_ht_order_porder_dref_off60) (-1
                                                                    - 2 * s V_cmd_put_ht_order_i
                                                                    - 2 * s V_cmd_put_ht_order_n
                                                                    + 2 * s V_cmd_put_ht_order_porder_dref_off60));
      (*0 0.5*) F_max0_ge_0 (-1 - 2 * s V_cmd_put_ht_order_i
                             - 2 * s V_cmd_put_ht_order_n
                             + 2 * s V_cmd_put_ht_order_porder_dref_off60);
      (*-0.00126577 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (199
                                                                    - s V_cmd_put_ht_order_n) (0))) (F_max0_ge_0 (199
                                                                    - s V_cmd_put_ht_order_n));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 - 2 * s V_cmd_put_ht_order_i
                                                                 + 2 * s V_cmd_put_ht_order_porder_dref_off60) (0))) (F_max0_ge_0 (-1
                                                                    - 2 * s V_cmd_put_ht_order_i
                                                                    + 2 * s V_cmd_put_ht_order_porder_dref_off60))]
     ((12478 # 127) - s V_cmd_put_ht_order_i
      - (0 # 1) * s V_cmd_put_ht_order_n
      + s V_cmd_put_ht_order_porder_dref_off60 + s V_cmd_put_ht_order_z
      - (0 # 1) * max0(199 - s V_cmd_put_ht_order_n) <= z)%Q
   | 53 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (198
                                                  - s V_cmd_put_ht_order_i
                                                  + s V_cmd_put_ht_order_porder_dref_off56)) (F_check_ge (198
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off56) (0))]
     (s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
      + max0(198 - s V_cmd_put_ht_order_i
             + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 54 => ((198 # 1) - s V_cmd_put_ht_order_i
            + s V_cmd_put_ht_order_porder_dref_off56 + s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 55 => ((198 # 1) - s V_cmd_put_ht_order_i
            + s V_cmd_put_ht_order_porder_dref_off56 + s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 56 => ((198 # 1) - s V_cmd_put_ht_order_i
            + s V_cmd_put_ht_order_porder_dref_off56 + s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 57 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (198
                                                               - s V_cmd_put_ht_order_i
                                                               + s V_cmd_put_ht_order_porder_dref_off56) (0))) (F_max0_ge_0 (198
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off56))]
     ((198 # 1) - s V_cmd_put_ht_order_i
      + s V_cmd_put_ht_order_porder_dref_off56 + s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 58 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (198
                                                              - s V_cmd_put_ht_order_i
                                                              + s V_cmd_put_ht_order_porder_dref_off56) (0))) (F_max0_ge_0 (198
                                                                    - s V_cmd_put_ht_order_i
                                                                    + s V_cmd_put_ht_order_porder_dref_off56))]
     ((198 # 1) - s V_cmd_put_ht_order_i
      + s V_cmd_put_ht_order_porder_dref_off56 + s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60) <= z)%Q
   | 59 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 60 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 61 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 62 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (198 - s V_cmd_put_ht_order_i
                                       + s V_cmd_put_ht_order_porder_dref_off56) (s V_cmd_put_ht_order_n);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_put_ht_order_n) (0))) (F_max0_ge_0 (s V_cmd_put_ht_order_n))]
     (s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
      + max0(198 - s V_cmd_put_ht_order_i
             + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 63 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 64 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 65 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 66 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 67 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 68 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 69 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_put_ht_order_n)) (F_check_ge (s V_cmd_put_ht_order_n) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_cmd_put_ht_order_n)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_cmd_put_ht_order_n) (0))) (F_max0_ge_0 (-1
                                                                    + s V_cmd_put_ht_order_n))]
     (-(1 # 1) + s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
      + max0(198 - s V_cmd_put_ht_order_i
             + s V_cmd_put_ht_order_porder_dref_off56)
      + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 71 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 72 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
                   + s V_cmd_put_ht_order_porder_dref_off56)
            + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 73 => hints
     [(*-1 0*) F_max0_ge_0 (98 + s V_cmd_put_ht_order_porder_dref_off60);
      (*-1 0*) F_max0_ge_0 (198 - s V_cmd_put_ht_order_i
                            - s V_cmd_put_ht_order_n
                            + s V_cmd_put_ht_order_porder_dref_off56);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_cmd_put_ht_order_n)) (F_check_ge (0) (0))]
     (s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
      + max0(198 - s V_cmd_put_ht_order_i - s V_cmd_put_ht_order_n
             + s V_cmd_put_ht_order_porder_dref_off56)
      + max0(s V_cmd_put_ht_order_n) <= z)%Q
   | 74 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 75 => (s V_cmd_put_ht_order_z
            + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
            + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 76 => hints
     [(*-1 0*) F_max0_ge_0 (98 + s V_cmd_put_ht_order_porder_dref_off60);
      (*-1 0*) F_max0_ge_0 (198 + s V_cmd_put_ht_order_porder_dref_off56)]
     (s V_cmd_put_ht_order_z
      + max0(98 + s V_cmd_put_ht_order_porder_dref_off60)
      + max0(198 + s V_cmd_put_ht_order_porder_dref_off56) <= z)%Q
   | 77 => (s V_cmd_put_ht_order_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_put_ht_order =>
    [mkPA Q (fun n z s => ai_cmd_put_ht_order n s /\ annot0_cmd_put_ht_order n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_put_ht_order (proc_start P_cmd_put_ht_order) s1 (proc_end P_cmd_put_ht_order) s2 ->
    (s2 V_cmd_put_ht_order_z <= max0(98
                                     + s1 V_cmd_put_ht_order_porder_dref_off60)
                                + max0(198
                                       + s1 V_cmd_put_ht_order_porder_dref_off56))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_put_ht_order.
Qed.
