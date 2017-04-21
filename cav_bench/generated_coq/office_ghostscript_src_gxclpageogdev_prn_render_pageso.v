Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gdev_prn_render_pages.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gdev_prn_render_pages_z := 1%positive.
Notation V_gdev_prn_render_pages__tmp := 2%positive.
Notation V_gdev_prn_render_pages__tmp1 := 3%positive.
Notation V_gdev_prn_render_pages_code := 4%positive.
Notation V_gdev_prn_render_pages_i := 5%positive.
Notation V_gdev_prn_render_pages_i1 := 6%positive.
Notation V_gdev_prn_render_pages_count := 7%positive.
Notation V_gdev_prn_render_pages_pdev := 8%positive.
Notation V_gdev_prn_render_pages_ppages := 9%positive.
Definition Pedges_gdev_prn_render_pages: list (edge proc) :=
  (EA 1 (AAssign V_gdev_prn_render_pages_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gdev_prn_render_pages__tmp
  (Some (EVar V_gdev_prn_render_pages_count))) 3)::(EA 3 (AAssign
  V_gdev_prn_render_pages_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_gdev_prn_render_pages_i) s) <
  (eval (EVar V_gdev_prn_render_pages__tmp) s))%Z)) 24)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_gdev_prn_render_pages_i) s) >=
  (eval (EVar V_gdev_prn_render_pages__tmp) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AAssign V_gdev_prn_render_pages_code None) 9)::(EA 9 (AAssign
  V_gdev_prn_render_pages_i1 (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_gdev_prn_render_pages_i1) s) <
  (eval (EVar V_gdev_prn_render_pages__tmp) s))%Z)) 17)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_gdev_prn_render_pages_i1) s) >=
  (eval (EVar V_gdev_prn_render_pages__tmp) s))%Z)) 13)::(EA 13 AWeaken 14)::
  (EA 14 (AAssign V_gdev_prn_render_pages__tmp1
  (Some (EVar V_gdev_prn_render_pages_code))) 15)::(EA 15 ANone 16)::
  (EA 16 AWeaken 59)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_gdev_prn_render_pages_i1 (Some (EAdd (EVar V_gdev_prn_render_pages_i1)
  (ENum (1))))) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_gdev_prn_render_pages_z (Some (EAdd (ENum (1))
  (EVar V_gdev_prn_render_pages_z)))) 23)::(EA 23 AWeaken 12)::
  (EA 24 AWeaken 25)::(EA 25 ANone 56)::(EA 25 ANone 26)::
  (EA 26 AWeaken 27)::(EA 27 ANone 56)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::(EA 29 ANone 53)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 ANone 50)::(EA 31 ANone 32)::
  (EA 32 AWeaken 33)::(EA 33 ANone 50)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_gdev_prn_render_pages_i) s) = (eval (ENum (0))
  s))%Z)) 42)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_gdev_prn_render_pages_i) s) <> (eval (ENum (0))
  s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 ANone 39)::(EA 37 ANone 38)::
  (EA 38 ANone 44)::(EA 39 (AAssign V_gdev_prn_render_pages__tmp1
  (Some (ENum (-15)))) 40)::(EA 40 ANone 41)::(EA 41 AWeaken 59)::
  (EA 42 AWeaken 43)::(EA 43 ANone 44)::(EA 44 ANone 45)::(EA 45 (AAssign
  V_gdev_prn_render_pages_i (Some (EAdd (EVar V_gdev_prn_render_pages_i)
  (ENum (1))))) 46)::(EA 46 ANone 47)::(EA 47 ANone 48)::(EA 48 (AAssign
  V_gdev_prn_render_pages_z (Some (EAdd (ENum (1))
  (EVar V_gdev_prn_render_pages_z)))) 49)::(EA 49 AWeaken 6)::(EA 50 (AAssign
  V_gdev_prn_render_pages__tmp1 (Some (ENum (-15)))) 51)::(EA 51 ANone 52)::
  (EA 52 AWeaken 59)::(EA 53 (AAssign V_gdev_prn_render_pages__tmp1
  (Some (ENum (-15)))) 54)::(EA 54 ANone 55)::(EA 55 AWeaken 59)::
  (EA 56 (AAssign V_gdev_prn_render_pages__tmp1 (Some (ENum (-15)))) 57)::
  (EA 57 ANone 58)::(EA 58 AWeaken 59)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gdev_prn_render_pages => Pedges_gdev_prn_render_pages
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gdev_prn_render_pages => 59
     end)%positive;
  var_global := var_global
}.

Definition ai_gdev_prn_render_pages (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0)%Z
   | 3 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages_z <= 0)%Z
   | 4 => (1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 5 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages_z <= 0)%Z
   | 6 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 7 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 8 => (1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 9 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 10 => (1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0)%Z
   | 11 => (-1 * s V_gdev_prn_render_pages_i1 <= 0 /\ 1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 12 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 13 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i1 <= 0)%Z
   | 14 => (1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 15 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i1 <= 0)%Z
   | 16 => (1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 17 => (1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 + 1 <= 0)%Z
   | 18 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 19 => (1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 + 1 <= 0)%Z
   | 20 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 <= 0)%Z
   | 21 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 + 1 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0)%Z
   | 22 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 <= 0)%Z
   | 23 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i1 + 1 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp+ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z + 1 <= 0)%Z
   | 24 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 25 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 26 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 27 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 28 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 29 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 30 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 31 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 32 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 33 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 34 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 35 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 36 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 37 => (-1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0)%Z
   | 38 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 39 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 40 => (-1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0)%Z
   | 41 => (-1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 42 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ 1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 43 => (1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 44 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 45 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | 46 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 47 => (-1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0)%Z
   | 48 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 49 => (-1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z + 1 <= 0)%Z
   | 50 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 51 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0)%Z
   | 52 => (-1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 53 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 54 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0)%Z
   | 55 => (-1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 56 => (-1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 57 => (-1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0)%Z
   | 58 => (-1 * s V_gdev_prn_render_pages__tmp1 + -15 <= 0 /\ 1 * s V_gdev_prn_render_pages__tmp1 + 15 <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0 /\ -1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages__tmp+ 1 * s V_gdev_prn_render_pages_i + 1 <= 0)%Z
   | 59 => (-1 * s V_gdev_prn_render_pages_z <= 0 /\ -1 * s V_gdev_prn_render_pages_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gdev_prn_render_pages (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) * max0(s V_gdev_prn_render_pages_count) <= z)%Q
   | 2 => (s V_gdev_prn_render_pages_z
           + (2 # 1) * max0(s V_gdev_prn_render_pages_count) <= z)%Q
   | 3 => (s V_gdev_prn_render_pages_z
           + (2 # 1) * max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 4 => (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
           + max0(s V_gdev_prn_render_pages__tmp
                  - s V_gdev_prn_render_pages_i) <= z)%Q
   | 5 => (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
           + max0(s V_gdev_prn_render_pages__tmp
                  - s V_gdev_prn_render_pages_i) <= z)%Q
   | 6 => (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
           + max0(s V_gdev_prn_render_pages__tmp
                  - s V_gdev_prn_render_pages_i) <= z)%Q
   | 7 => (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
           + max0(s V_gdev_prn_render_pages__tmp
                  - s V_gdev_prn_render_pages_i) <= z)%Q
   | 8 => (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
           + max0(s V_gdev_prn_render_pages__tmp
                  - s V_gdev_prn_render_pages_i) <= z)%Q
   | 9 => (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
           + max0(s V_gdev_prn_render_pages__tmp
                  - s V_gdev_prn_render_pages_i) <= z)%Q
   | 10 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 11 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 12 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 13 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 14 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 15 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gdev_prn_render_pages__tmp
                                             - s V_gdev_prn_render_pages_i) (-1
                                                                    + s V_gdev_prn_render_pages__tmp
                                                                    - s V_gdev_prn_render_pages_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gdev_prn_render_pages__tmp
                            - s V_gdev_prn_render_pages_i);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_gdev_prn_render_pages__tmp
                                             - s V_gdev_prn_render_pages_i1) (-1
                                                                    + s V_gdev_prn_render_pages__tmp
                                                                    - s V_gdev_prn_render_pages_i1));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gdev_prn_render_pages__tmp
                            - s V_gdev_prn_render_pages_i1)]
     (s V_gdev_prn_render_pages_z
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i)
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_gdev_prn_render_pages__tmp
                                       - s V_gdev_prn_render_pages_i1) (1)]
     (s V_gdev_prn_render_pages_z
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i)
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 18 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 19 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 20 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 21 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 22 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 23 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i1) <= z)%Q
   | 24 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 25 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 26 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 27 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 28 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_gdev_prn_render_pages__tmp
                                      - s V_gdev_prn_render_pages_i) (1)]
     (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i) <= z)%Q
   | 29 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 30 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 31 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 32 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 33 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 34 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_prn_render_pages__tmp)) (F_check_ge (s V_gdev_prn_render_pages__tmp) (0))]
     ((1 # 1) + s V_gdev_prn_render_pages_z
      + max0(-1 + s V_gdev_prn_render_pages__tmp
             - s V_gdev_prn_render_pages_i)
      + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 35 => ((1 # 1) + s V_gdev_prn_render_pages__tmp
            + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_prn_render_pages__tmp) (0))) (F_max0_ge_0 (s V_gdev_prn_render_pages__tmp))]
     ((1 # 1) + s V_gdev_prn_render_pages__tmp + s V_gdev_prn_render_pages_z
      + max0(-1 + s V_gdev_prn_render_pages__tmp
             - s V_gdev_prn_render_pages_i) <= z)%Q
   | 37 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 38 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 39 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 40 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_gdev_prn_render_pages__tmp
                            - s V_gdev_prn_render_pages_i);
      (*-1 0*) F_max0_ge_0 (s V_gdev_prn_render_pages__tmp)]
     ((1 # 1) + s V_gdev_prn_render_pages_z
      + max0(-1 + s V_gdev_prn_render_pages__tmp
             - s V_gdev_prn_render_pages_i)
      + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_prn_render_pages__tmp) (0))) (F_max0_ge_0 (s V_gdev_prn_render_pages__tmp))]
     ((1 # 1) + s V_gdev_prn_render_pages__tmp + s V_gdev_prn_render_pages_z
      + max0(-1 + s V_gdev_prn_render_pages__tmp
             - s V_gdev_prn_render_pages_i) <= z)%Q
   | 43 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 44 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 45 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 46 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 47 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 48 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_prn_render_pages_z)) (F_check_ge (s V_gdev_prn_render_pages_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_prn_render_pages_z) (0))) (F_max0_ge_0 (s V_gdev_prn_render_pages_z))]
     (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i) <= z)%Q
   | 50 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 51 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 52 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (s V_gdev_prn_render_pages__tmp);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                + s V_gdev_prn_render_pages__tmp
                                                - s V_gdev_prn_render_pages_i)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_gdev_prn_render_pages_z
      + max0(-1 + s V_gdev_prn_render_pages__tmp
             - s V_gdev_prn_render_pages_i)
      + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 53 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 54 => ((1 # 1) + s V_gdev_prn_render_pages_z
            + max0(-1 + s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i)
            + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 55 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_gdev_prn_render_pages__tmp
                            - s V_gdev_prn_render_pages_i);
      (*-1 0*) F_max0_ge_0 (s V_gdev_prn_render_pages__tmp)]
     ((1 # 1) + s V_gdev_prn_render_pages_z
      + max0(-1 + s V_gdev_prn_render_pages__tmp
             - s V_gdev_prn_render_pages_i)
      + max0(s V_gdev_prn_render_pages__tmp) <= z)%Q
   | 56 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 57 => (s V_gdev_prn_render_pages_z
            + max0(s V_gdev_prn_render_pages__tmp)
            + max0(s V_gdev_prn_render_pages__tmp
                   - s V_gdev_prn_render_pages_i) <= z)%Q
   | 58 => hints
     [(*-2 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_gdev_prn_render_pages__tmp
                                       - s V_gdev_prn_render_pages_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_gdev_prn_render_pages__tmp
                            - s V_gdev_prn_render_pages_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_prn_render_pages__tmp)) (F_check_ge (s V_gdev_prn_render_pages__tmp) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gdev_prn_render_pages__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gdev_prn_render_pages__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gdev_prn_render_pages__tmp))]
     (s V_gdev_prn_render_pages_z + max0(s V_gdev_prn_render_pages__tmp)
      + max0(s V_gdev_prn_render_pages__tmp - s V_gdev_prn_render_pages_i) <= z)%Q
   | 59 => (s V_gdev_prn_render_pages_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gdev_prn_render_pages =>
    [mkPA Q (fun n z s => ai_gdev_prn_render_pages n s /\ annot0_gdev_prn_render_pages n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gdev_prn_render_pages (proc_start P_gdev_prn_render_pages) s1 (proc_end P_gdev_prn_render_pages) s2 ->
    (s2 V_gdev_prn_render_pages_z <= (2 # 1) * max0(s1 V_gdev_prn_render_pages_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gdev_prn_render_pages.
Qed.
