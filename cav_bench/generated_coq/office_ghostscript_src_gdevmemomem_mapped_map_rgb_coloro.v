Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mem_mapped_map_rgb_color.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mem_mapped_map_rgb_color_z := 1%positive.
Notation V_mem_mapped_map_rgb_color__tmp := 2%positive.
Notation V_mem_mapped_map_rgb_color__tmp1 := 3%positive.
Notation V_mem_mapped_map_rgb_color__tmp2 := 4%positive.
Notation V_mem_mapped_map_rgb_color_bb := 5%positive.
Notation V_mem_mapped_map_rgb_color_best := 6%positive.
Notation V_mem_mapped_map_rgb_color_bg := 7%positive.
Notation V_mem_mapped_map_rgb_color_br := 8%positive.
Notation V_mem_mapped_map_rgb_color_cnt := 9%positive.
Notation V_mem_mapped_map_rgb_color_db := 10%positive.
Notation V_mem_mapped_map_rgb_color_dev_dref_off552_off8 := 11%positive.
Notation V_mem_mapped_map_rgb_color_dg := 12%positive.
Notation V_mem_mapped_map_rgb_color_diff := 13%positive.
Notation V_mem_mapped_map_rgb_color_b := 14%positive.
Notation V_mem_mapped_map_rgb_color_dev := 15%positive.
Notation V_mem_mapped_map_rgb_color_g := 16%positive.
Notation V_mem_mapped_map_rgb_color_r := 17%positive.
Definition Pedges_mem_mapped_map_rgb_color: list (edge proc) :=
  (EA 1 (AAssign V_mem_mapped_map_rgb_color_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_mem_mapped_map_rgb_color__tmp2
  (Some (EVar V_mem_mapped_map_rgb_color_r))) 3)::(EA 3 (AAssign
  V_mem_mapped_map_rgb_color__tmp1
  (Some (EVar V_mem_mapped_map_rgb_color_g))) 4)::(EA 4 (AAssign
  V_mem_mapped_map_rgb_color__tmp
  (Some (EVar V_mem_mapped_map_rgb_color_b))) 5)::(EA 5 (AAssign
  V_mem_mapped_map_rgb_color_br None) 6)::(EA 6 (AAssign
  V_mem_mapped_map_rgb_color_bg None) 7)::(EA 7 (AAssign
  V_mem_mapped_map_rgb_color_bb None) 8)::(EA 8 (AAssign
  V_mem_mapped_map_rgb_color_cnt
  (Some (EVar V_mem_mapped_map_rgb_color_dev_dref_off552_off8))) 9)::
  (EA 9 (AAssign V_mem_mapped_map_rgb_color_best (Some (ENum (768)))) 10)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_mem_mapped_map_rgb_color_cnt
  (Some (ESub (EVar V_mem_mapped_map_rgb_color_cnt) (ENum (3))))) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (ESub (EVar V_mem_mapped_map_rgb_color_cnt) (ENum (3)))
  s) >= (eval (ENum (0)) s))%Z)) 16)::(EA 13 (AGuard
  (fun s => ((eval (ESub (EVar V_mem_mapped_map_rgb_color_cnt) (ENum (3)))
  s) < (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 16 AWeaken 17)::
  (EA 17 (AAssign V_mem_mapped_map_rgb_color_diff None) 18)::
  (EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_diff) s) <
  (eval (ENum (0)) s))%Z)) 21)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_diff) s) >=
  (eval (ENum (0)) s))%Z)) 20)::(EA 20 AWeaken 25)::(EA 21 AWeaken 22)::
  (EA 22 (AAssign V_mem_mapped_map_rgb_color_diff (Some (ESub (ENum (0))
  (EVar V_mem_mapped_map_rgb_color_diff)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_diff) s) <
  (eval (EVar V_mem_mapped_map_rgb_color_best) s))%Z)) 27)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_diff) s) >=
  (eval (EVar V_mem_mapped_map_rgb_color_best) s))%Z)) 26)::
  (EA 26 AWeaken 56)::(EA 27 AWeaken 28)::(EA 28 (AAssign
  V_mem_mapped_map_rgb_color_dg None) 29)::(EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_dg) s) < (eval (ENum (0))
  s))%Z)) 32)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_dg) s) >=
  (eval (ENum (0)) s))%Z)) 31)::(EA 31 AWeaken 35)::(EA 32 AWeaken 33)::
  (EA 33 (AAssign V_mem_mapped_map_rgb_color_dg (Some (ESub (ENum (0))
  (EVar V_mem_mapped_map_rgb_color_dg)))) 34)::(EA 34 ANone 35)::
  (EA 35 (AAssign V_mem_mapped_map_rgb_color_diff
  (Some (EAdd (EVar V_mem_mapped_map_rgb_color_diff)
  (EVar V_mem_mapped_map_rgb_color_dg)))) 36)::(EA 36 AWeaken 37)::
  (EA 37 (AGuard
  (fun s => ((eval (EAdd (EVar V_mem_mapped_map_rgb_color_diff)
  (EVar V_mem_mapped_map_rgb_color_dg)) s) <
  (eval (EVar V_mem_mapped_map_rgb_color_best) s))%Z)) 39)::(EA 37 (AGuard
  (fun s => ((eval (EAdd (EVar V_mem_mapped_map_rgb_color_diff)
  (EVar V_mem_mapped_map_rgb_color_dg)) s) >=
  (eval (EVar V_mem_mapped_map_rgb_color_best) s))%Z)) 38)::
  (EA 38 AWeaken 55)::(EA 39 AWeaken 40)::(EA 40 (AAssign
  V_mem_mapped_map_rgb_color_db None) 41)::(EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_db) s) < (eval (ENum (0))
  s))%Z)) 44)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_mem_mapped_map_rgb_color_db) s) >=
  (eval (ENum (0)) s))%Z)) 43)::(EA 43 AWeaken 47)::(EA 44 AWeaken 45)::
  (EA 45 (AAssign V_mem_mapped_map_rgb_color_db (Some (ESub (ENum (0))
  (EVar V_mem_mapped_map_rgb_color_db)))) 46)::(EA 46 ANone 47)::
  (EA 47 (AAssign V_mem_mapped_map_rgb_color_diff
  (Some (EAdd (EVar V_mem_mapped_map_rgb_color_diff)
  (EVar V_mem_mapped_map_rgb_color_db)))) 48)::(EA 48 AWeaken 49)::
  (EA 49 (AGuard
  (fun s => ((eval (EAdd (EVar V_mem_mapped_map_rgb_color_diff)
  (EVar V_mem_mapped_map_rgb_color_db)) s) <
  (eval (EVar V_mem_mapped_map_rgb_color_best) s))%Z)) 51)::(EA 49 (AGuard
  (fun s => ((eval (EAdd (EVar V_mem_mapped_map_rgb_color_diff)
  (EVar V_mem_mapped_map_rgb_color_db)) s) >=
  (eval (EVar V_mem_mapped_map_rgb_color_best) s))%Z)) 50)::
  (EA 50 AWeaken 54)::(EA 51 AWeaken 52)::(EA 52 (AAssign
  V_mem_mapped_map_rgb_color_best
  (Some (EVar V_mem_mapped_map_rgb_color_diff))) 53)::(EA 53 ANone 54)::
  (EA 54 ANone 55)::(EA 55 ANone 56)::(EA 56 ANone 57)::(EA 57 ANone 58)::
  (EA 58 (AAssign V_mem_mapped_map_rgb_color_z (Some (EAdd (ENum (1))
  (EVar V_mem_mapped_map_rgb_color_z)))) 11)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mem_mapped_map_rgb_color => Pedges_mem_mapped_map_rgb_color
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mem_mapped_map_rgb_color => 15
     end)%positive;
  var_global := var_global
}.

Definition ai_mem_mapped_map_rgb_color (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 3 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 4 => (1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 5 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 6 => (1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 7 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 8 => (1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 9 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 10 => (1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_best + -768 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best + 768 <= 0)%Z
   | 11 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 12 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 13 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 14 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_cnt + -2 <= 0)%Z
   | 15 => (1 * s V_mem_mapped_map_rgb_color_cnt + -2 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 16 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 17 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 18 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 19 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 20 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0)%Z
   | 21 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 22 => (1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 23 => (-1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 24 => (-1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0)%Z
   | 25 => (-1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 26 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0)%Z
   | 27 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 28 => (-1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 29 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 30 => (-1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 31 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0)%Z
   | 32 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg + 1 <= 0)%Z
   | 33 => (1 * s V_mem_mapped_map_rgb_color_dg + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 34 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg + 1 <= 0)%Z
   | 35 => (-1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 36 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0)%Z
   | 37 => (1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 38 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0)%Z
   | 39 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 40 => (-1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 41 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 42 => (-1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 43 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0)%Z
   | 44 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_db + 1 <= 0)%Z
   | 45 => (1 * s V_mem_mapped_map_rgb_color_db + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 46 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db + 1 <= 0)%Z
   | 47 => (-1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 48 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ -1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 49 => (1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 50 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0)%Z
   | 51 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0)%Z
   | 52 => (-1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ -1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ 1 * s V_mem_mapped_map_rgb_color_diff + 1 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 53 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_best+ 1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg <= 0)%Z
   | 54 => (1 * s V_mem_mapped_map_rgb_color_db+ 1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_db <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 55 => (1 * s V_mem_mapped_map_rgb_color_dg+ -1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_dg <= 0)%Z
   | 56 => (-1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | 57 => (-1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_diff <= 0)%Z
   | 58 => (-1 * s V_mem_mapped_map_rgb_color_diff <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_z <= 0 /\ -1 * s V_mem_mapped_map_rgb_color_cnt + 3 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mem_mapped_map_rgb_color (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 3) * max0(-3
                          + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 2 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 3 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 4 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 5 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 6 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 7 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 8 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3
                            + s V_mem_mapped_map_rgb_color_dev_dref_off552_off8) <= z)%Q
   | 9 => (s V_mem_mapped_map_rgb_color_z
           + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 10 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 11 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 12 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 13 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 14 => hints
     [(*-0.333333 0*) F_max0_monotonic (F_check_ge (s V_mem_mapped_map_rgb_color_cnt) (-3
                                                                    + s V_mem_mapped_map_rgb_color_cnt));
      (*-0.333333 0*) F_max0_ge_0 (-3 + s V_mem_mapped_map_rgb_color_cnt)]
     (s V_mem_mapped_map_rgb_color_z
      + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 15 => (s V_mem_mapped_map_rgb_color_z <= z)%Q
   | 16 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 17 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 18 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 19 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 20 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 21 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 22 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 23 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 24 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 25 => (s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 26 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (s V_mem_mapped_map_rgb_color_cnt) (3)]
     (s V_mem_mapped_map_rgb_color_z
      + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 27 => hints
     [(*0 0.333333*) F_max0_pre_decrement 1 (s V_mem_mapped_map_rgb_color_cnt) (3)]
     (s V_mem_mapped_map_rgb_color_z
      + (1 # 3) * max0(s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 28 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 29 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 30 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 31 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 32 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 33 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 34 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 35 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 36 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 37 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 38 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 39 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 40 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 41 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 42 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 43 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 44 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 45 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 46 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 47 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 48 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 49 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 50 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 51 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 52 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 53 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 54 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 55 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 56 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 57 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | 58 => ((1 # 1) + s V_mem_mapped_map_rgb_color_z
            + (1 # 3) * max0(-3 + s V_mem_mapped_map_rgb_color_cnt) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mem_mapped_map_rgb_color =>
    [mkPA Q (fun n z s => ai_mem_mapped_map_rgb_color n s /\ annot0_mem_mapped_map_rgb_color n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mem_mapped_map_rgb_color (proc_start P_mem_mapped_map_rgb_color) s1 (proc_end P_mem_mapped_map_rgb_color) s2 ->
    (s2 V_mem_mapped_map_rgb_color_z <= (1 # 3) * max0(-3
                                                       + s1 V_mem_mapped_map_rgb_color_dev_dref_off552_off8))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mem_mapped_map_rgb_color.
Qed.
