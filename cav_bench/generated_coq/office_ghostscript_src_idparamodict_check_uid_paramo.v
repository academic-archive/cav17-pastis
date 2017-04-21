Require Import pasta.Pasta.

Inductive proc: Type :=
  P_dict_check_uid_param.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_dict_check_uid_param_z := 1%positive.
Notation V_dict_check_uid_param__tmp := 2%positive.
Notation V_dict_check_uid_param_i := 3%positive.
Notation V_dict_check_uid_param_puid_dref_off0 := 4%positive.
Notation V_dict_check_uid_param_size := 5%positive.
Notation V_dict_check_uid_param_pdict := 6%positive.
Notation V_dict_check_uid_param_puid := 7%positive.
Definition Pedges_dict_check_uid_param: list (edge proc) :=
  (EA 1 (AAssign V_dict_check_uid_param_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_dict_check_uid_param_size) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_dict_check_uid_param_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_dict_check_uid_param_puid_dref_off0) s) <
  (eval (ENum (0)) s))%Z)) 17)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_dict_check_uid_param_puid_dref_off0) s) >=
  (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 14)::
  (EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 9 ANone 11)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_dict_check_uid_param__tmp None) 12)::
  (EA 12 ANone 13)::(EA 13 AWeaken 55)::(EA 14 (AAssign
  V_dict_check_uid_param__tmp (Some (ENum (0)))) 15)::(EA 15 ANone 16)::
  (EA 16 AWeaken 55)::(EA 17 AWeaken 18)::(EA 18 (AAssign
  V_dict_check_uid_param_size (Some (ESub (ENum (0))
  (EVar V_dict_check_uid_param_puid_dref_off0)))) 19)::(EA 19 AWeaken 20)::
  (EA 20 ANone 52)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 ANone 23)::
  (EA 22 ANone 49)::(EA 23 AWeaken 24)::(EA 24 ANone 49)::(EA 24 ANone 25)::
  (EA 25 (AAssign V_dict_check_uid_param_i (Some (ENum (0)))) 26)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_dict_check_uid_param_i) s) <
  (eval (EVar V_dict_check_uid_param_size) s))%Z)) 33)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_dict_check_uid_param_i) s) >=
  (eval (EVar V_dict_check_uid_param_size) s))%Z)) 29)::(EA 29 AWeaken 30)::
  (EA 30 (AAssign V_dict_check_uid_param__tmp (Some (ENum (1)))) 31)::
  (EA 31 ANone 32)::(EA 32 AWeaken 55)::(EA 33 AWeaken 34)::
  (EA 34 ANone 38)::(EA 34 ANone 35)::(EA 35 (AAssign
  V_dict_check_uid_param__tmp (Some (ENum (0)))) 36)::(EA 36 ANone 37)::
  (EA 37 AWeaken 55)::(EA 38 AWeaken 39)::(EA 39 ANone 46)::
  (EA 39 ANone 40)::(EA 40 ANone 41)::(EA 41 (AAssign
  V_dict_check_uid_param_i (Some (EAdd (EVar V_dict_check_uid_param_i)
  (ENum (1))))) 42)::(EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_dict_check_uid_param_z (Some (EAdd (ENum (1))
  (EVar V_dict_check_uid_param_z)))) 45)::(EA 45 AWeaken 28)::(EA 46 (AAssign
  V_dict_check_uid_param__tmp (Some (ENum (0)))) 47)::(EA 47 ANone 48)::
  (EA 48 AWeaken 55)::(EA 49 (AAssign V_dict_check_uid_param__tmp
  (Some (ENum (0)))) 50)::(EA 50 ANone 51)::(EA 51 AWeaken 55)::
  (EA 52 (AAssign V_dict_check_uid_param__tmp (Some (ENum (0)))) 53)::
  (EA 53 ANone 54)::(EA 54 AWeaken 55)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_dict_check_uid_param => Pedges_dict_check_uid_param
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_dict_check_uid_param => 55
     end)%positive;
  var_global := var_global
}.

Definition ai_dict_check_uid_param (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0)%Z
   | 3 => (-1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 4 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0)%Z
   | 5 => (-1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 6 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_puid_dref_off0 <= 0)%Z
   | 7 => (-1 * s V_dict_check_uid_param_puid_dref_off0 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 8 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_puid_dref_off0 <= 0)%Z
   | 9 => (-1 * s V_dict_check_uid_param_puid_dref_off0 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 10 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_puid_dref_off0 <= 0)%Z
   | 11 => (-1 * s V_dict_check_uid_param_puid_dref_off0 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 12 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_puid_dref_off0 <= 0)%Z
   | 13 => (-1 * s V_dict_check_uid_param_puid_dref_off0 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 14 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_puid_dref_off0 <= 0)%Z
   | 15 => (-1 * s V_dict_check_uid_param_puid_dref_off0 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ -1 * s V_dict_check_uid_param__tmp <= 0)%Z
   | 16 => (-1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_puid_dref_off0 <= 0)%Z
   | 17 => (-1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0)%Z
   | 18 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 19 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 20 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0)%Z
   | 21 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 22 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0)%Z
   | 23 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 24 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0)%Z
   | 25 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 26 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0)%Z
   | 27 => (-1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 28 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 29 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i+ 1 * s V_dict_check_uid_param_size <= 0)%Z
   | 30 => (-1 * s V_dict_check_uid_param_i+ 1 * s V_dict_check_uid_param_size <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 31 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i+ 1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param__tmp + -1 <= 0 /\ -1 * s V_dict_check_uid_param__tmp + 1 <= 0)%Z
   | 32 => (-1 * s V_dict_check_uid_param__tmp + 1 <= 0 /\ 1 * s V_dict_check_uid_param__tmp + -1 <= 0 /\ -1 * s V_dict_check_uid_param_i+ 1 * s V_dict_check_uid_param_size <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0)%Z
   | 33 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 34 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0)%Z
   | 35 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 36 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ -1 * s V_dict_check_uid_param__tmp <= 0)%Z
   | 37 => (-1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 38 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 39 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0)%Z
   | 40 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 41 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0)%Z
   | 42 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0 /\ -1 * s V_dict_check_uid_param_i + 1 <= 0)%Z
   | 43 => (-1 * s V_dict_check_uid_param_i + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0)%Z
   | 44 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0 /\ -1 * s V_dict_check_uid_param_i + 1 <= 0)%Z
   | 45 => (-1 * s V_dict_check_uid_param_i + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z + 1 <= 0)%Z
   | 46 => (1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 47 => (1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ -1 * s V_dict_check_uid_param__tmp <= 0)%Z
   | 48 => (-1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_i+ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 49 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 50 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ -1 * s V_dict_check_uid_param__tmp <= 0)%Z
   | 51 => (-1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 52 => (1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 53 => (-1 * s V_dict_check_uid_param_size + 1 <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ -1 * s V_dict_check_uid_param__tmp <= 0)%Z
   | 54 => (-1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param__tmp <= 0 /\ 1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ 1 * s V_dict_check_uid_param_puid_dref_off0 + 1 <= 0 /\ -1 * s V_dict_check_uid_param_size + 1 <= 0)%Z
   | 55 => (-1 * s V_dict_check_uid_param_size <= 0 /\ -1 * s V_dict_check_uid_param_i <= 0 /\ -1 * s V_dict_check_uid_param_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_dict_check_uid_param (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 2 => (max0(-s V_dict_check_uid_param_puid_dref_off0)
           + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 3 => (max0(-s V_dict_check_uid_param_puid_dref_off0)
           + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 4 => (max0(-s V_dict_check_uid_param_puid_dref_off0)
           + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 5 => (max0(-s V_dict_check_uid_param_puid_dref_off0)
           + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_dict_check_uid_param_z)) (F_check_ge (s V_dict_check_uid_param_z) (0))]
     (max0(-s V_dict_check_uid_param_puid_dref_off0)
      + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 7 => (s V_dict_check_uid_param_z
           + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 8 => (s V_dict_check_uid_param_z
           + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 9 => (s V_dict_check_uid_param_z
           + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 10 => (s V_dict_check_uid_param_z
            + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 11 => (s V_dict_check_uid_param_z
            + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 12 => (s V_dict_check_uid_param_z
            + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_ge_0 (-s V_dict_check_uid_param_puid_dref_off0)]
     (s V_dict_check_uid_param_z
      + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 14 => (s V_dict_check_uid_param_z
            + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 15 => (s V_dict_check_uid_param_z
            + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_ge_0 (-s V_dict_check_uid_param_puid_dref_off0)]
     (s V_dict_check_uid_param_z
      + max0(-s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 17 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              - s V_dict_check_uid_param_puid_dref_off0) (0))) (F_max0_ge_0 (-1
                                                                    - s V_dict_check_uid_param_puid_dref_off0))]
     (max0(-s V_dict_check_uid_param_puid_dref_off0)
      + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 18 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
            + max0(-s V_dict_check_uid_param_puid_dref_off0)
            + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 19 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_dict_check_uid_param_z)) (F_check_ge (s V_dict_check_uid_param_z) (0))]
     ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
      + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
      + max0(s V_dict_check_uid_param_size)
      + max0(s V_dict_check_uid_param_z) <= z)%Q
   | 20 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
            + max0(s V_dict_check_uid_param_size) <= z)%Q
   | 21 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_dict_check_uid_param_size)) (F_check_ge (s V_dict_check_uid_param_size) (0))]
     ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
      + s V_dict_check_uid_param_z
      + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
      + max0(s V_dict_check_uid_param_size) <= z)%Q
   | 22 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 23 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 24 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 25 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 26 => ((1 # 1) - (1 # 2) * s V_dict_check_uid_param_i
            + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   - s V_dict_check_uid_param_puid_dref_off0)) (F_check_ge (-1
                                                                    - s V_dict_check_uid_param_puid_dref_off0) (0))]
     ((1 # 1) - (1 # 2) * s V_dict_check_uid_param_i
      + s V_dict_check_uid_param_puid_dref_off0
      + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
      + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
      + (1 # 2) * max0(-s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 28 => (-(1 # 2) * s V_dict_check_uid_param_i
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 29 => (-(1 # 2) * s V_dict_check_uid_param_i
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 30 => (-(1 # 2) * s V_dict_check_uid_param_i
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 31 => (-(1 # 2) * s V_dict_check_uid_param_i
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_dict_check_uid_param_i
                                             + s V_dict_check_uid_param_size) (-1
                                                                    - s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size));
      (*-1 0*) F_max0_ge_0 (-1 - s V_dict_check_uid_param_i
                            + s V_dict_check_uid_param_size);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (s V_dict_check_uid_param_size));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_dict_check_uid_param_i
                                                                 + s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (-
                                                                    s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size))]
     (-(1 # 2) * s V_dict_check_uid_param_i + s V_dict_check_uid_param_size
      + s V_dict_check_uid_param_z
      + (1 # 2) * max0(-s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 33 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                - s V_dict_check_uid_param_i
                                                                + s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (-1
                                                                    - s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size))]
     (-(1 # 2) * s V_dict_check_uid_param_i + s V_dict_check_uid_param_size
      + s V_dict_check_uid_param_z
      + (1 # 2) * max0(-s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 34 => ((1 # 2) + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 35 => ((1 # 2) + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 36 => (-(1 # 2) + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            + max0(1 - s V_dict_check_uid_param__tmp)
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size)
            - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 37 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (-s V_dict_check_uid_param_i
                                         + s V_dict_check_uid_param_size) (1);
      (*-1 0*) F_max0_ge_0 (-1 - s V_dict_check_uid_param_i
                            + s V_dict_check_uid_param_size);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (s V_dict_check_uid_param_size));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_dict_check_uid_param__tmp)) (F_check_ge (0) (0))]
     (-(1 # 2) + (1 # 2) * s V_dict_check_uid_param_size
      + s V_dict_check_uid_param_z
      + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      + max0(1 - s V_dict_check_uid_param__tmp)
      + (1 # 2) * max0(-s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 38 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (s V_dict_check_uid_param_size));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_dict_check_uid_param_i
                                                     + s V_dict_check_uid_param_size)) (F_check_ge (-
                                                                    s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size) (0))]
     ((1 # 2) + (1 # 2) * s V_dict_check_uid_param_size
      + s V_dict_check_uid_param_z
      + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      + (1 # 2) * max0(-s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size)
      - (1 # 2) * max0(s V_dict_check_uid_param_size) <= z)%Q
   | 39 => ((1 # 2) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 40 => ((1 # 2) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 41 => ((1 # 2) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 42 => ((1 # 1) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 43 => ((1 # 1) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 44 => ((1 # 1) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 45 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_dict_check_uid_param_size)) (F_check_ge (s V_dict_check_uid_param_size) (0))]
     (-(1 # 2) * s V_dict_check_uid_param_i
      + (1 # 2) * s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
      + (1 # 2) * max0(-s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size) <= z)%Q
   | 46 => ((1 # 2) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 47 => ((1 # 2) - (1 # 2) * s V_dict_check_uid_param_i
            + (1 # 2) * s V_dict_check_uid_param_size
            + s V_dict_check_uid_param_z
            + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                             + s V_dict_check_uid_param_size) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_dict_check_uid_param_i
                                             + s V_dict_check_uid_param_size) (-1
                                                                    - s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size));
      (*-1 0*) F_max0_ge_0 (-1 - s V_dict_check_uid_param_i
                            + s V_dict_check_uid_param_size);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_dict_check_uid_param_i
                                                               + s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (-
                                                                    s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     - s V_dict_check_uid_param_i
                                                     + s V_dict_check_uid_param_size)) (F_check_ge (-1
                                                                    - s V_dict_check_uid_param_i
                                                                    + s V_dict_check_uid_param_size) (0))]
     ((1 # 2) - (1 # 2) * s V_dict_check_uid_param_i
      + (1 # 2) * s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
      + (1 # 2) * max0(-1 - s V_dict_check_uid_param_i
                       + s V_dict_check_uid_param_size) <= z)%Q
   | 49 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 50 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_ge_0 (s V_dict_check_uid_param_size);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_dict_check_uid_param_size) (0))) (F_max0_ge_0 (s V_dict_check_uid_param_size));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   - s V_dict_check_uid_param_puid_dref_off0)) (F_check_ge (-1
                                                                    - s V_dict_check_uid_param_puid_dref_off0) (0))]
     ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
      + s V_dict_check_uid_param_size + s V_dict_check_uid_param_z
      + max0(-1 - s V_dict_check_uid_param_puid_dref_off0) <= z)%Q
   | 52 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
            + max0(s V_dict_check_uid_param_size) <= z)%Q
   | 53 => ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
            + s V_dict_check_uid_param_z
            + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
            + max0(s V_dict_check_uid_param_size) <= z)%Q
   | 54 => hints
     [(*-1 0*) F_max0_ge_0 (s V_dict_check_uid_param_size);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   - s V_dict_check_uid_param_puid_dref_off0)) (F_check_ge (-1
                                                                    - s V_dict_check_uid_param_puid_dref_off0) (0))]
     ((1 # 1) + s V_dict_check_uid_param_puid_dref_off0
      + s V_dict_check_uid_param_z
      + max0(-1 - s V_dict_check_uid_param_puid_dref_off0)
      + max0(s V_dict_check_uid_param_size) <= z)%Q
   | 55 => (s V_dict_check_uid_param_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_dict_check_uid_param =>
    [mkPA Q (fun n z s => ai_dict_check_uid_param n s /\ annot0_dict_check_uid_param n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_dict_check_uid_param (proc_start P_dict_check_uid_param) s1 (proc_end P_dict_check_uid_param) s2 ->
    (s2 V_dict_check_uid_param_z <= max0(-s1 V_dict_check_uid_param_puid_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_dict_check_uid_param.
Qed.
