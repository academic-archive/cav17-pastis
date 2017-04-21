Require Import pasta.Pasta.

Inductive proc: Type :=
  P_init_trust_lst.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_init_trust_lst_z := 1%positive.
Notation V_init_trust_lst_i := 2%positive.
Notation V_init_trust_lst_init_trust_lst_dot_initialized := 3%positive.
Notation V_init_trust_lst_legitlst_len := 4%positive.
Notation V_init_trust_lst_len := 5%positive.
Notation V_init_trust_lst_trustlst_len := 6%positive.
Definition Pedges_init_trust_lst: list (edge proc) :=
  (EA 1 (AAssign V_init_trust_lst_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_init_trust_lst_dot_initialized)
  s) <> (eval (ENum (0)) s))%Z)) 53)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_init_trust_lst_dot_initialized)
  s) = (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_init_trust_lst_i (Some (ENum (0)))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard (fun s => ((eval (EVar V_init_trust_lst_i)
  s) < (eval (ENum (8)) s))%Z)) 34)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_i) s) >= (eval (ENum (8))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 (AAssign V_init_trust_lst_i
  (Some (ENum (0)))) 11)::(EA 11 ANone 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_init_trust_lst_i) s) <
  (eval (ENum (4)) s))%Z)) 18)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_i) s) >= (eval (ENum (4))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AAssign
  V_init_trust_lst_init_trust_lst_dot_initialized (Some (ENum (1)))) 16)::
  (EA 16 ANone 17)::(EA 17 AWeaken 56)::(EA 18 AWeaken 19)::
  (EA 19 ANone 20)::(EA 19 ANone 21)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_init_trust_lst_len None) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_len) s) >
  (eval (EVar V_init_trust_lst_legitlst_len) s))%Z)) 25)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_len) s) <=
  (eval (EVar V_init_trust_lst_legitlst_len) s))%Z)) 24)::
  (EA 24 AWeaken 28)::(EA 25 AWeaken 26)::(EA 26 (AAssign
  V_init_trust_lst_legitlst_len (Some (EVar V_init_trust_lst_len))) 27)::
  (EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign V_init_trust_lst_i
  (Some (EAdd (EVar V_init_trust_lst_i) (ENum (1))))) 30)::(EA 30 ANone 31)::
  (EA 31 ANone 32)::(EA 32 (AAssign V_init_trust_lst_z (Some (EAdd (ENum (1))
  (EVar V_init_trust_lst_z)))) 33)::(EA 33 AWeaken 13)::(EA 34 AWeaken 35)::
  (EA 35 ANone 36)::(EA 35 ANone 47)::(EA 36 AWeaken 37)::(EA 37 ANone 38)::
  (EA 37 ANone 39)::(EA 38 ANone 39)::(EA 39 (AAssign V_init_trust_lst_len
  None) 40)::(EA 40 AWeaken 41)::(EA 41 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_len) s) >
  (eval (EVar V_init_trust_lst_trustlst_len) s))%Z)) 43)::(EA 41 (AGuard
  (fun s => ((eval (EVar V_init_trust_lst_len) s) <=
  (eval (EVar V_init_trust_lst_trustlst_len) s))%Z)) 42)::
  (EA 42 AWeaken 46)::(EA 43 AWeaken 44)::(EA 44 (AAssign
  V_init_trust_lst_trustlst_len (Some (EVar V_init_trust_lst_len))) 45)::
  (EA 45 ANone 46)::(EA 46 ANone 47)::(EA 47 ANone 48)::(EA 48 (AAssign
  V_init_trust_lst_i (Some (EAdd (EVar V_init_trust_lst_i)
  (ENum (1))))) 49)::(EA 49 ANone 50)::(EA 50 ANone 51)::(EA 51 (AAssign
  V_init_trust_lst_z (Some (EAdd (ENum (1))
  (EVar V_init_trust_lst_z)))) 52)::(EA 52 AWeaken 8)::(EA 53 AWeaken 54)::
  (EA 54 ANone 55)::(EA 55 AWeaken 56)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_init_trust_lst => Pedges_init_trust_lst
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_init_trust_lst => 56
     end)%positive;
  var_global := var_global
}.

Definition ai_init_trust_lst (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_z <= 0)%Z
   | 3 => (-1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_z <= 0)%Z
   | 4 => (1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 5 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_z <= 0)%Z
   | 6 => (1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_i <= 0)%Z
   | 7 => (-1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_z <= 0)%Z
   | 8 => (-1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_i + -8 <= 0)%Z
   | 9 => (1 * s V_init_trust_lst_i + -8 <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i + 8 <= 0)%Z
   | 10 => (-1 * s V_init_trust_lst_i + 8 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_i + -8 <= 0)%Z
   | 11 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_i <= 0)%Z
   | 12 => (-1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 13 => (-1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_i + -4 <= 0)%Z
   | 14 => (1 * s V_init_trust_lst_i + -4 <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i + 4 <= 0)%Z
   | 15 => (-1 * s V_init_trust_lst_i + 4 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_i + -4 <= 0)%Z
   | 16 => (1 * s V_init_trust_lst_i + -4 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i + 4 <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized + -1 <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized + 1 <= 0)%Z
   | 17 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized + 1 <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized + -1 <= 0 /\ -1 * s V_init_trust_lst_i + 4 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -4 <= 0)%Z
   | 18 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0)%Z
   | 19 => (1 * s V_init_trust_lst_i + -3 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 20 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0)%Z
   | 21 => (1 * s V_init_trust_lst_i + -3 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 22 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0)%Z
   | 23 => (1 * s V_init_trust_lst_i + -3 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 24 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0 /\ -1 * s V_init_trust_lst_legitlst_len+ 1 * s V_init_trust_lst_len <= 0)%Z
   | 25 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0 /\ 1 * s V_init_trust_lst_legitlst_len+ -1 * s V_init_trust_lst_len + 1 <= 0)%Z
   | 26 => (1 * s V_init_trust_lst_legitlst_len+ -1 * s V_init_trust_lst_len + 1 <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 27 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0)%Z
   | 28 => (1 * s V_init_trust_lst_i + -3 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 29 => (-1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -3 <= 0)%Z
   | 30 => (-1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i + 1 <= 0 /\ 1 * s V_init_trust_lst_i + -4 <= 0)%Z
   | 31 => (1 * s V_init_trust_lst_i + -4 <= 0 /\ -1 * s V_init_trust_lst_i + 1 <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0)%Z
   | 32 => (-1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i + 1 <= 0 /\ 1 * s V_init_trust_lst_i + -4 <= 0)%Z
   | 33 => (1 * s V_init_trust_lst_i + -4 <= 0 /\ -1 * s V_init_trust_lst_i + 1 <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z + 1 <= 0)%Z
   | 34 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0)%Z
   | 35 => (1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 36 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0)%Z
   | 37 => (1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 38 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0)%Z
   | 39 => (1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 40 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0)%Z
   | 41 => (1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 42 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0 /\ 1 * s V_init_trust_lst_len+ -1 * s V_init_trust_lst_trustlst_len <= 0)%Z
   | 43 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_len+ 1 * s V_init_trust_lst_trustlst_len + 1 <= 0)%Z
   | 44 => (-1 * s V_init_trust_lst_len+ 1 * s V_init_trust_lst_trustlst_len + 1 <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 45 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0)%Z
   | 46 => (1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 47 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -7 <= 0)%Z
   | 48 => (1 * s V_init_trust_lst_i + -7 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_i <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 49 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -8 <= 0 /\ -1 * s V_init_trust_lst_i + 1 <= 0)%Z
   | 50 => (-1 * s V_init_trust_lst_i + 1 <= 0 /\ 1 * s V_init_trust_lst_i + -8 <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0)%Z
   | 51 => (1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_i + -8 <= 0 /\ -1 * s V_init_trust_lst_i + 1 <= 0)%Z
   | 52 => (-1 * s V_init_trust_lst_i + 1 <= 0 /\ 1 * s V_init_trust_lst_i + -8 <= 0 /\ -1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ 1 * s V_init_trust_lst_init_trust_lst_dot_initialized <= 0 /\ -1 * s V_init_trust_lst_z + 1 <= 0)%Z
   | 53 => (1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_z <= 0)%Z
   | 54 => (-1 * s V_init_trust_lst_z <= 0 /\ 1 * s V_init_trust_lst_z <= 0)%Z
   | 55 => (1 * s V_init_trust_lst_z <= 0 /\ -1 * s V_init_trust_lst_z <= 0)%Z
   | 56 => (-1 * s V_init_trust_lst_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_init_trust_lst (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((12 # 1) <= z)%Q
   | 2 => ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 3 => ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 4 => ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 5 => ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 6 => ((4 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 7 => ((4 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 8 => ((4 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_init_trust_lst_i) (7
                                                                    - s V_init_trust_lst_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_init_trust_lst_i)]
     ((4 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 10 => ((4 # 1) + s V_init_trust_lst_z <= z)%Q
   | 11 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 12 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 13 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 14 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 15 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 16 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_init_trust_lst_i) (3
                                                                    - s V_init_trust_lst_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_init_trust_lst_i)]
     (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 18 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 19 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 20 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 21 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 22 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 23 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 24 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_init_trust_lst_i) (1)]
     (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_init_trust_lst_i) (1)]
     (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 26 => ((1 # 1) + s V_init_trust_lst_z + max0(3 - s V_init_trust_lst_i) <= z)%Q
   | 27 => ((1 # 1) + s V_init_trust_lst_z + max0(3 - s V_init_trust_lst_i) <= z)%Q
   | 28 => ((1 # 1) + s V_init_trust_lst_z + max0(3 - s V_init_trust_lst_i) <= z)%Q
   | 29 => ((1 # 1) + s V_init_trust_lst_z + max0(3 - s V_init_trust_lst_i) <= z)%Q
   | 30 => ((1 # 1) + s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 31 => ((1 # 1) + s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 32 => ((1 # 1) + s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 33 => (s V_init_trust_lst_z + max0(4 - s V_init_trust_lst_i) <= z)%Q
   | 34 => hints
     [(*0 1*) F_max0_pre_decrement 1 (8 - s V_init_trust_lst_i) (1)]
     ((4 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 35 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 36 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 37 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 38 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 39 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 40 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 41 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 42 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 43 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 44 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 45 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 46 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 47 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 48 => ((5 # 1) + s V_init_trust_lst_z + max0(7 - s V_init_trust_lst_i) <= z)%Q
   | 49 => ((5 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 50 => ((5 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 51 => ((5 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 52 => ((4 # 1) + s V_init_trust_lst_z + max0(8 - s V_init_trust_lst_i) <= z)%Q
   | 53 => ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 54 => ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 55 => hints
     [(*-12 0*) F_one]
     ((12 # 1) + s V_init_trust_lst_z <= z)%Q
   | 56 => (s V_init_trust_lst_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_init_trust_lst =>
    [mkPA Q (fun n z s => ai_init_trust_lst n s /\ annot0_init_trust_lst n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_init_trust_lst (proc_start P_init_trust_lst) s1 (proc_end P_init_trust_lst) s2 ->
    (s2 V_init_trust_lst_z <= (12 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_init_trust_lst.
Qed.
