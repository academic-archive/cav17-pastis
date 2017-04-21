Require Import pasta.Pasta.

Inductive proc: Type :=
  P_compute_scalefacs_long.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_compute_scalefacs_long_z := 1%positive.
Notation V_compute_scalefacs_long_cod_info_dref_off64 := 2%positive.
Notation V_compute_scalefacs_long_ifqstep_inv := 3%positive.
Notation V_compute_scalefacs_long_sfb := 4%positive.
Notation V_compute_scalefacs_long_cod_info := 5%positive.
Notation V_compute_scalefacs_long_scalefac := 6%positive.
Notation V_compute_scalefacs_long_vbrsf := 7%positive.
Definition Pedges_compute_scalefacs_long: list (edge proc) :=
  (EA 1 (AAssign V_compute_scalefacs_long_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_compute_scalefacs_long_ifqstep_inv None) 3)::
  (EA 3 (AAssign V_compute_scalefacs_long_cod_info_dref_off64
  (Some (ENum (0)))) 4)::(EA 4 (AAssign V_compute_scalefacs_long_sfb
  (Some (ENum (11)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) < (eval (ENum (21))
  s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) >=
  (eval (ENum (21)) s))%Z)) 8)::(EA 8 AWeaken 19)::(EA 9 AWeaken 10)::
  (EA 10 ANone 17)::(EA 10 ANone 11)::(EA 11 ANone 12)::(EA 12 (AAssign
  V_compute_scalefacs_long_sfb
  (Some (EAdd (EVar V_compute_scalefacs_long_sfb) (ENum (1))))) 13)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_compute_scalefacs_long_z (Some (EAdd (ENum (1))
  (EVar V_compute_scalefacs_long_z)))) 16)::(EA 16 AWeaken 7)::
  (EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) = (eval (ENum (21))
  s))%Z)) 21)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) <>
  (eval (ENum (21)) s))%Z)) 20)::(EA 20 AWeaken 29)::(EA 21 AWeaken 22)::
  (EA 22 (AAssign V_compute_scalefacs_long_cod_info_dref_off64
  (Some (ENum (1)))) 23)::(EA 23 (AAssign V_compute_scalefacs_long_sfb
  (Some (ENum (11)))) 24)::(EA 24 ANone 25)::(EA 25 AWeaken 26)::
  (EA 26 (AGuard (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) <
  (eval (ENum (21)) s))%Z)) 51)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) >=
  (eval (ENum (21)) s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::
  (EA 29 (AAssign V_compute_scalefacs_long_sfb (Some (ENum (0)))) 30)::
  (EA 30 ANone 31)::(EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) < (eval (ENum (21))
  s))%Z)) 35)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) >=
  (eval (ENum (21)) s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 35 AWeaken 36)::
  (EA 36 (AGuard (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) <
  (eval (ENum (11)) s))%Z)) 40)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_long_sfb) s) >=
  (eval (ENum (11)) s))%Z)) 37)::(EA 37 AWeaken 38)::(EA 38 ANone 39)::
  (EA 39 AWeaken 43)::(EA 40 AWeaken 41)::(EA 41 ANone 42)::
  (EA 42 AWeaken 43)::(EA 43 ANone 44)::(EA 43 ANone 45)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_compute_scalefacs_long_sfb
  (Some (EAdd (EVar V_compute_scalefacs_long_sfb) (ENum (1))))) 47)::
  (EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 (AAssign
  V_compute_scalefacs_long_z (Some (EAdd (ENum (1))
  (EVar V_compute_scalefacs_long_z)))) 50)::(EA 50 AWeaken 32)::
  (EA 51 AWeaken 52)::(EA 52 ANone 53)::(EA 53 (AAssign
  V_compute_scalefacs_long_sfb
  (Some (EAdd (EVar V_compute_scalefacs_long_sfb) (ENum (1))))) 54)::
  (EA 54 ANone 55)::(EA 55 ANone 56)::(EA 56 (AAssign
  V_compute_scalefacs_long_z (Some (EAdd (ENum (1))
  (EVar V_compute_scalefacs_long_z)))) 57)::(EA 57 AWeaken 26)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_compute_scalefacs_long => Pedges_compute_scalefacs_long
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_compute_scalefacs_long => 34
     end)%positive;
  var_global := var_global
}.

Definition ai_compute_scalefacs_long (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 3 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 4 => (1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 5 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -11 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0)%Z
   | 6 => (-1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -11 <= 0 /\ 1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 7 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 8 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 21 <= 0)%Z
   | 9 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 10 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 11 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 12 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 13 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 12 <= 0)%Z
   | 14 => (-1 * s V_compute_scalefacs_long_sfb + 12 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 15 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 12 <= 0)%Z
   | 16 => (-1 * s V_compute_scalefacs_long_sfb + 12 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_z + 1 <= 0)%Z
   | 17 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 18 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 19 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 20 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 21 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 21 <= 0)%Z
   | 22 => (-1 * s V_compute_scalefacs_long_sfb + 21 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 23 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 21 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0)%Z
   | 24 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -11 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0)%Z
   | 25 => (-1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0)%Z
   | 26 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 27 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 21 <= 0)%Z
   | 28 => (-1 * s V_compute_scalefacs_long_sfb + 21 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 29 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 30 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0)%Z
   | 31 => (-1 * s V_compute_scalefacs_long_sfb <= 0 /\ 1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 32 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 33 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 21 <= 0)%Z
   | 34 => (-1 * s V_compute_scalefacs_long_sfb + 21 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 35 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 36 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 37 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0)%Z
   | 38 => (-1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 39 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0)%Z
   | 40 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -10 <= 0)%Z
   | 41 => (1 * s V_compute_scalefacs_long_sfb + -10 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 42 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -10 <= 0)%Z
   | 43 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 44 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 45 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0)%Z
   | 46 => (-1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 47 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 48 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 49 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 50 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_z + 1 <= 0)%Z
   | 51 => (1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 52 => (1 * s V_compute_scalefacs_long_sfb + -20 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0)%Z
   | 53 => (1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 11 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -20 <= 0)%Z
   | 54 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 12 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 55 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 12 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_z <= 0)%Z
   | 56 => (-1 * s V_compute_scalefacs_long_z <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 12 <= 0 /\ 1 * s V_compute_scalefacs_long_sfb + -21 <= 0)%Z
   | 57 => (1 * s V_compute_scalefacs_long_sfb + -21 <= 0 /\ -1 * s V_compute_scalefacs_long_sfb + 12 <= 0 /\ 1 * s V_compute_scalefacs_long_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_compute_scalefacs_long_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_compute_scalefacs_long_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_compute_scalefacs_long (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((41 # 1) <= z)%Q
   | 2 => ((41 # 1) + s V_compute_scalefacs_long_z <= z)%Q
   | 3 => ((41 # 1) + s V_compute_scalefacs_long_z <= z)%Q
   | 4 => ((20 # 1)
           + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
           + s V_compute_scalefacs_long_z
           + (21 # 1) * max0(1
                             - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 5 => ((31 # 1)
           + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
           - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
           + (21 # 1) * max0(1
                             - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 6 => ((31 # 1)
           + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
           - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
           + (21 # 1) * max0(1
                             - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 7 => ((31 # 1)
           + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
           - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
           + (21 # 1) * max0(1
                             - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 8 => ((31 # 1)
           + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
           - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
           + (21 # 1) * max0(1
                             - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 9 => ((31 # 1)
           + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
           - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
           + (21 # 1) * max0(1
                             - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 10 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 11 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 12 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 13 => ((32 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 14 => ((32 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 15 => ((32 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 16 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 17 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 18 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 19 => ((31 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 20 => hints
     [(*0 11*) F_one;
      (*-1 0*) F_max0_ge_0 (20 - s V_compute_scalefacs_long_sfb);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                               - s V_compute_scalefacs_long_sfb) (0))) (F_max0_ge_0 (20
                                                                    - s V_compute_scalefacs_long_sfb))]
     ((31 # 1) + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
      - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
      + (21 # 1) * max0(1 - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                             - s V_compute_scalefacs_long_sfb) (20
                                                                    - s V_compute_scalefacs_long_sfb));
      (*-1 0*) F_max0_ge_0 (20 - s V_compute_scalefacs_long_sfb);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                              - s V_compute_scalefacs_long_sfb) (0))) (F_max0_ge_0 (21
                                                                    - s V_compute_scalefacs_long_sfb));
      (*-31 -10*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                      - s V_compute_scalefacs_long_cod_info_dref_off64)) (F_check_ge (1
                                                                    - s V_compute_scalefacs_long_cod_info_dref_off64) (0))]
     ((31 # 1) + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
      - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
      + (21 # 1) * max0(1 - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 22 => ((31 # 1) + s V_compute_scalefacs_long_z <= z)%Q
   | 23 => ((10 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 24 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 25 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 26 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                             - s V_compute_scalefacs_long_sfb) (20
                                                                    - s V_compute_scalefacs_long_sfb));
      (*-1 0*) F_max0_ge_0 (20 - s V_compute_scalefacs_long_sfb)]
     ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
      + s V_compute_scalefacs_long_z
      + (21 # 1) * max0(1 - s V_compute_scalefacs_long_cod_info_dref_off64)
      + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 28 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 29 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 30 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                               - s V_compute_scalefacs_long_sfb) (0))) (F_max0_ge_0 (21
                                                                    - s V_compute_scalefacs_long_sfb));
      (*-21 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                    - s V_compute_scalefacs_long_cod_info_dref_off64)) (F_check_ge (1
                                                                    - s V_compute_scalefacs_long_cod_info_dref_off64) (0))]
     ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
      - s V_compute_scalefacs_long_sfb + s V_compute_scalefacs_long_z
      + (21 # 1) * max0(1 - s V_compute_scalefacs_long_cod_info_dref_off64) <= z)%Q
   | 32 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (21
                                             - s V_compute_scalefacs_long_sfb) (20
                                                                    - s V_compute_scalefacs_long_sfb));
      (*-1 0*) F_max0_ge_0 (20 - s V_compute_scalefacs_long_sfb)]
     (s V_compute_scalefacs_long_z
      + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 34 => (s V_compute_scalefacs_long_z <= z)%Q
   | 35 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 36 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 37 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 38 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 39 => hints
     [(*0 1*) F_max0_pre_decrement 1 (21 - s V_compute_scalefacs_long_sfb) (1)]
     (s V_compute_scalefacs_long_z
      + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 40 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 41 => (s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (21 - s V_compute_scalefacs_long_sfb) (1)]
     (s V_compute_scalefacs_long_z
      + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 43 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(20 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 44 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(20 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 45 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(20 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 46 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(20 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 47 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 48 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 49 => ((1 # 1) + s V_compute_scalefacs_long_z
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_compute_scalefacs_long_z)) (F_check_ge (s V_compute_scalefacs_long_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compute_scalefacs_long_z) (0))) (F_max0_ge_0 (s V_compute_scalefacs_long_z))]
     (s V_compute_scalefacs_long_z
      + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (21 - s V_compute_scalefacs_long_sfb) (1)]
     ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
      + s V_compute_scalefacs_long_z
      + (21 # 1) * max0(1 - s V_compute_scalefacs_long_cod_info_dref_off64)
      + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 52 => ((1 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(20 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 53 => ((1 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(20 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 54 => ((1 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 55 => ((1 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 56 => ((1 # 1)
            + (21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | 57 => ((21 # 1) * s V_compute_scalefacs_long_cod_info_dref_off64
            + s V_compute_scalefacs_long_z
            + (21 # 1) * max0(1
                              - s V_compute_scalefacs_long_cod_info_dref_off64)
            + max0(21 - s V_compute_scalefacs_long_sfb) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_compute_scalefacs_long =>
    [mkPA Q (fun n z s => ai_compute_scalefacs_long n s /\ annot0_compute_scalefacs_long n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_compute_scalefacs_long (proc_start P_compute_scalefacs_long) s1 (proc_end P_compute_scalefacs_long) s2 ->
    (s2 V_compute_scalefacs_long_z <= (41 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_compute_scalefacs_long.
Qed.
