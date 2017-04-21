Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_hybrid.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_hybrid_z := 1%positive.
Notation V_III_hybrid__tmp := 2%positive.
Notation V_III_hybrid_b := 3%positive.
Notation V_III_hybrid_bt := 4%positive.
Notation V_III_hybrid_gr_info_dref_off16 := 5%positive.
Notation V_III_hybrid_gr_info_dref_off20 := 6%positive.
Notation V_III_hybrid_gr_info_dref_off64 := 7%positive.
Notation V_III_hybrid_i := 8%positive.
Notation V_III_hybrid_sb := 9%positive.
Notation V_III_hybrid_ch := 10%positive.
Notation V_III_hybrid_fsIn := 11%positive.
Notation V_III_hybrid_gr_info := 12%positive.
Notation V_III_hybrid_tsOut := 13%positive.
Definition Pedges_III_hybrid: list (edge proc) :=
  (EA 1 (AAssign V_III_hybrid_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_sb) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_III_hybrid_gr_info_dref_off64) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_III_hybrid__tmp (Some (EVar V_III_hybrid_ch))) 6)::(EA 6 (AAssign
  V_III_hybrid_sb (Some (ENum (0)))) 7)::(EA 7 (AAssign V_III_hybrid_b
  None) 8)::(EA 8 (AAssign V_III_hybrid_b (Some (EAdd (ESub (ENum (0))
  (EVar V_III_hybrid_b)) (ENum (1))))) 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_gr_info_dref_off20) s) <>
  (eval (ENum (0)) s))%Z)) 12)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_gr_info_dref_off20) s) =
  (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 15)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_III_hybrid_sb (Some (ENum (2)))) 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_III_hybrid_bt
  (Some (EVar V_III_hybrid_gr_info_dref_off16))) 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_III_hybrid_bt) s) =
  (eval (ENum (2)) s))%Z)) 31)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_bt) s) <> (eval (ENum (2))
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::
  (EA 21 (AGuard (fun s => ((eval (EVar V_III_hybrid_sb) s) <
  (eval (EVar V_III_hybrid_gr_info_dref_off64) s))%Z)) 24)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_sb) s) >=
  (eval (EVar V_III_hybrid_gr_info_dref_off64) s))%Z)) 22)::
  (EA 22 AWeaken 23)::(EA 23 ANone 37)::(EA 24 AWeaken 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_III_hybrid_sb
  (Some (EAdd (EVar V_III_hybrid_sb) (ENum (2))))) 27)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V_III_hybrid_z (Some (EAdd (ENum (1))
  (EVar V_III_hybrid_z)))) 30)::(EA 30 AWeaken 21)::(EA 31 AWeaken 32)::
  (EA 32 ANone 33)::(EA 33 AWeaken 34)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_sb) s) <
  (eval (EVar V_III_hybrid_gr_info_dref_off64) s))%Z)) 61)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_sb) s) >=
  (eval (EVar V_III_hybrid_gr_info_dref_off64) s))%Z)) 35)::
  (EA 35 AWeaken 36)::(EA 36 ANone 37)::(EA 37 ANone 38)::
  (EA 38 AWeaken 39)::(EA 39 (AGuard (fun s => ((eval (EVar V_III_hybrid_sb)
  s) < (eval (ENum (32)) s))%Z)) 42)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_sb) s) >= (eval (ENum (32))
  s))%Z)) 40)::(EA 40 AWeaken 41)::(EA 42 AWeaken 43)::(EA 43 (AAssign
  V_III_hybrid_i (Some (ENum (0)))) 44)::(EA 44 ANone 45)::
  (EA 45 AWeaken 46)::(EA 46 (AGuard (fun s => ((eval (EVar V_III_hybrid_i)
  s) < (eval (ENum (18)) s))%Z)) 54)::(EA 46 (AGuard
  (fun s => ((eval (EVar V_III_hybrid_i) s) >= (eval (ENum (18))
  s))%Z)) 47)::(EA 47 AWeaken 48)::(EA 48 ANone 49)::(EA 49 (AAssign
  V_III_hybrid_sb (Some (EAdd (EVar V_III_hybrid_sb) (ENum (1))))) 50)::
  (EA 50 ANone 51)::(EA 51 ANone 52)::(EA 52 (AAssign V_III_hybrid_z
  (Some (EAdd (ENum (1)) (EVar V_III_hybrid_z)))) 53)::(EA 53 AWeaken 39)::
  (EA 54 AWeaken 55)::(EA 55 ANone 56)::(EA 56 (AAssign V_III_hybrid_i
  (Some (EAdd (EVar V_III_hybrid_i) (ENum (1))))) 57)::(EA 57 ANone 58)::
  (EA 58 ANone 59)::(EA 59 (AAssign V_III_hybrid_z (Some (EAdd (ENum (1))
  (EVar V_III_hybrid_z)))) 60)::(EA 60 AWeaken 46)::(EA 61 AWeaken 62)::
  (EA 62 ANone 63)::(EA 63 (AAssign V_III_hybrid_sb
  (Some (EAdd (EVar V_III_hybrid_sb) (ENum (2))))) 64)::(EA 64 ANone 65)::
  (EA 65 ANone 66)::(EA 66 (AAssign V_III_hybrid_z (Some (EAdd (ENum (1))
  (EVar V_III_hybrid_z)))) 67)::(EA 67 AWeaken 34)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_hybrid => Pedges_III_hybrid
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_hybrid => 41
     end)%positive;
  var_global := var_global
}.

Definition ai_III_hybrid (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0)%Z
   | 3 => (-1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 4 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 5 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 6 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 7 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 8 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 9 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 10 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 11 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off20 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off20 <= 0)%Z
   | 12 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 13 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 14 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0)%Z
   | 15 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 16 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 17 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 18 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 19 => (-1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 20 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ -1 * s V_III_hybrid_sb <= 0)%Z
   | 21 => (-1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 22 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0)%Z
   | 23 => (1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 24 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + 1 <= 0)%Z
   | 25 => (-1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + 1 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0)%Z
   | 26 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + 1 <= 0)%Z
   | 27 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0)%Z
   | 28 => (-1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ -1 * s V_III_hybrid_z <= 0)%Z
   | 29 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0)%Z
   | 30 => (-1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ -1 * s V_III_hybrid_z + 1 <= 0)%Z
   | 31 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0)%Z
   | 32 => (-1 * s V_III_hybrid_bt + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 33 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -2 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0)%Z
   | 34 => (-1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0)%Z
   | 35 => (1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0)%Z
   | 36 => (1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0)%Z
   | 37 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0)%Z
   | 38 => (1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 39 => (-1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 40 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb + 32 <= 0)%Z
   | 41 => (-1 * s V_III_hybrid_sb + 32 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 42 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0)%Z
   | 43 => (1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 44 => (-1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ 1 * s V_III_hybrid_i <= 0 /\ -1 * s V_III_hybrid_i <= 0)%Z
   | 45 => (-1 * s V_III_hybrid_i <= 0 /\ 1 * s V_III_hybrid_i <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0)%Z
   | 46 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_i <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0)%Z
   | 47 => (1 * s V_III_hybrid_i + -18 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_i + 18 <= 0)%Z
   | 48 => (-1 * s V_III_hybrid_i + 18 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0)%Z
   | 49 => (1 * s V_III_hybrid_i + -18 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_i + 18 <= 0)%Z
   | 50 => (-1 * s V_III_hybrid_i + 18 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0 /\ 1 * s V_III_hybrid_sb + -32 <= 0 /\ -1 * s V_III_hybrid_sb + 1 <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb + 1 <= 0)%Z
   | 51 => (1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb + 1 <= 0 /\ -1 * s V_III_hybrid_sb + 1 <= 0 /\ 1 * s V_III_hybrid_sb + -32 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_i + 18 <= 0)%Z
   | 52 => (-1 * s V_III_hybrid_i + 18 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0 /\ 1 * s V_III_hybrid_sb + -32 <= 0 /\ -1 * s V_III_hybrid_sb + 1 <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb + 1 <= 0)%Z
   | 53 => (1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb + 1 <= 0 /\ -1 * s V_III_hybrid_sb + 1 <= 0 /\ 1 * s V_III_hybrid_sb + -32 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_i + 18 <= 0 /\ -1 * s V_III_hybrid_z + 1 <= 0)%Z
   | 54 => (1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_i <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_i + -17 <= 0)%Z
   | 55 => (1 * s V_III_hybrid_i + -17 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_i <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0)%Z
   | 56 => (1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_i <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_i + -17 <= 0)%Z
   | 57 => (-1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_i + 1 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0)%Z
   | 58 => (1 * s V_III_hybrid_i + -18 <= 0 /\ -1 * s V_III_hybrid_i + 1 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0)%Z
   | 59 => (-1 * s V_III_hybrid_z <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_i + 1 <= 0 /\ 1 * s V_III_hybrid_i + -18 <= 0)%Z
   | 60 => (1 * s V_III_hybrid_i + -18 <= 0 /\ -1 * s V_III_hybrid_i + 1 <= 0 /\ 1 * s V_III_hybrid_sb + -31 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ 1 * s V_III_hybrid_gr_info_dref_off64+ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z + 1 <= 0)%Z
   | 61 => (1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + 1 <= 0)%Z
   | 62 => (-1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + 1 <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0)%Z
   | 63 => (1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ -1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_sb <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + 1 <= 0)%Z
   | 64 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0)%Z
   | 65 => (-1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ -1 * s V_III_hybrid_z <= 0)%Z
   | 66 => (-1 * s V_III_hybrid_z <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ -1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0)%Z
   | 67 => (-1 * s V_III_hybrid_gr_info_dref_off64+ 1 * s V_III_hybrid_sb + -1 <= 0 /\ -1 * s V_III_hybrid_sb + 2 <= 0 /\ 1 * s V_III_hybrid_bt + -2 <= 0 /\ -1 * s V_III_hybrid_bt + 2 <= 0 /\ -1 * s V_III_hybrid_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_hybrid (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((608 # 1) + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 2 => ((608 # 1) + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 3 => ((608 # 1) + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 4 => ((608 # 1) + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 5 => ((608 # 1) + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 6 => ((608 # 1) + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 7 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
           + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                            - s V_III_hybrid_sb)
           + (266 # 15) * max0(s V_III_hybrid_sb) <= z)%Q
   | 8 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
           + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                            - s V_III_hybrid_sb)
           + (266 # 15) * max0(s V_III_hybrid_sb) <= z)%Q
   | 9 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
           + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
           + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                            - s V_III_hybrid_sb)
           + (266 # 15) * max0(s V_III_hybrid_sb) <= z)%Q
   | 10 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (266 # 15) * max0(s V_III_hybrid_sb) <= z)%Q
   | 11 => hints
     [(*0 17.7333*) F_binom_monotonic 1 (F_max0_ge_0 (s V_III_hybrid_sb)) (F_check_ge (0) (0));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                  + s V_III_hybrid_gr_info_dref_off64)) (F_check_ge (0) (0))]
     ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
      + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb)
      + (266 # 15) * max0(s V_III_hybrid_sb) <= z)%Q
   | 12 => hints
     [(*0 1.26667*) F_max0_pre_decrement 1 (32 - s V_III_hybrid_sb) (2);
      (*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_III_hybrid_gr_info_dref_off64
                                               - s V_III_hybrid_sb) (-1
                                                                    + 
                                                                    s V_III_hybrid_gr_info_dref_off64
                                                                    - 
                                                                    s V_III_hybrid_sb));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_III_hybrid_gr_info_dref_off64
                              - s V_III_hybrid_sb);
      (*-1.26667 0*) F_max0_ge_0 (30 - s V_III_hybrid_sb);
      (*-17.7333 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_hybrid_sb)) (F_check_ge (s V_III_hybrid_sb) (0));
      (*0 1.26667*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                    - 
                                                                    s V_III_hybrid_sb) (0))) (F_max0_ge_0 (32
                                                                    - s V_III_hybrid_sb))]
     ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
      + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64)
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb)
      + (266 # 15) * max0(s V_III_hybrid_sb) <= z)%Q
   | 13 => ((570 # 1)
            + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64) <= z)%Q
   | 14 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) <= z)%Q
   | 15 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) <= z)%Q
   | 16 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) <= z)%Q
   | 17 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) <= z)%Q
   | 18 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_III_hybrid_z) (0))) (F_max0_ge_0 (-
                                                                    s V_III_hybrid_z))]
     ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb) <= z)%Q
   | 19 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) + max0(-s V_III_hybrid_z) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_III_hybrid_z)) (F_check_ge (0) (0));
      (*-19 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                - s V_III_hybrid_sb) (0))) (F_max0_ge_0 (32
                                                                    - s V_III_hybrid_sb))]
     ((608 # 1) - (19 # 1) * s V_III_hybrid_sb + s V_III_hybrid_z
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb) + max0(-s V_III_hybrid_z) <= z)%Q
   | 21 => (s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 22 => hints
     [(*0 0.5*) F_max0_monotonic (F_check_ge (1
                                              + s V_III_hybrid_gr_info_dref_off64
                                              - s V_III_hybrid_sb) (-1
                                                                    + 
                                                                    s V_III_hybrid_gr_info_dref_off64
                                                                    - 
                                                                    s V_III_hybrid_sb));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                   + s V_III_hybrid_gr_info_dref_off64
                                                   - s V_III_hybrid_sb)) (F_check_ge (0) (0))]
     (s V_III_hybrid_z
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 23 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 24 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_III_hybrid_gr_info_dref_off64
                                         - s V_III_hybrid_sb) (2);
      (*-19 0*) F_max0_monotonic (F_check_ge (32 - s V_III_hybrid_sb) (30
                                                                    - s V_III_hybrid_sb))]
     (s V_III_hybrid_z
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 25 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(30 - s V_III_hybrid_sb) <= z)%Q
   | 26 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(30 - s V_III_hybrid_sb) <= z)%Q
   | 27 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 28 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 29 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 30 => (s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 31 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) <= z)%Q
   | 32 => ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_III_hybrid_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_III_hybrid_z) (0))) (F_max0_ge_0 (-
                                                                    s V_III_hybrid_z));
      (*-19 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                - s V_III_hybrid_sb) (0))) (F_max0_ge_0 (32
                                                                    - s V_III_hybrid_sb))]
     ((608 # 1) - (19 # 1) * s V_III_hybrid_sb
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb) <= z)%Q
   | 34 => (s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 35 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_III_hybrid_gr_info_dref_off64
                                               - s V_III_hybrid_sb) (-1
                                                                    + 
                                                                    s V_III_hybrid_gr_info_dref_off64
                                                                    - 
                                                                    s V_III_hybrid_sb));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_III_hybrid_gr_info_dref_off64
                              - s V_III_hybrid_sb)]
     (s V_III_hybrid_z
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 36 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 37 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 38 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 39 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 40 => hints
     [(*-19 0*) F_max0_monotonic (F_check_ge (32 - s V_III_hybrid_sb) (30
                                                                    - s V_III_hybrid_sb));
      (*-19 0*) F_max0_ge_0 (30 - s V_III_hybrid_sb)]
     (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 41 => (s V_III_hybrid_z <= z)%Q
   | 42 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 43 => (s V_III_hybrid_z + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 44 => (-(18 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 45 => (-(18 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 46 => (-(18 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 47 => hints
     [(*-19 0*) F_max0_pre_decrement 1 (32 - s V_III_hybrid_sb) (1)]
     (-(18 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 48 => ((1 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(31 - s V_III_hybrid_sb) <= z)%Q
   | 49 => ((1 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(31 - s V_III_hybrid_sb) <= z)%Q
   | 50 => ((1 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 51 => ((1 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 52 => ((1 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 53 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (18 - s V_III_hybrid_i) (17
                                                                    - 
                                                                    s V_III_hybrid_i));
      (*-1 0*) F_max0_ge_0 (17 - s V_III_hybrid_i)]
     (s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 54 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (18 - s V_III_hybrid_i)) (F_check_ge (18
                                                                    - s V_III_hybrid_i) (0))]
     (-(18 # 1) + s V_III_hybrid_z + max0(18 - s V_III_hybrid_i)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 55 => (-s V_III_hybrid_i + s V_III_hybrid_z
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 56 => (-s V_III_hybrid_i + s V_III_hybrid_z
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 57 => ((1 # 1) - s V_III_hybrid_i + s V_III_hybrid_z
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 58 => ((1 # 1) - s V_III_hybrid_i + s V_III_hybrid_z
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 59 => ((1 # 1) - s V_III_hybrid_i + s V_III_hybrid_z
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 60 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18
                                                               - s V_III_hybrid_i) (0))) (F_max0_ge_0 (18
                                                                    - s V_III_hybrid_i))]
     (-s V_III_hybrid_i + s V_III_hybrid_z
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 61 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_III_hybrid_gr_info_dref_off64
                                         - s V_III_hybrid_sb) (2);
      (*-19 0*) F_max0_monotonic (F_check_ge (32 - s V_III_hybrid_sb) (30
                                                                    - s V_III_hybrid_sb))]
     (s V_III_hybrid_z
      + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                       - s V_III_hybrid_sb)
      + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 62 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(30 - s V_III_hybrid_sb) <= z)%Q
   | 63 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(-1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(30 - s V_III_hybrid_sb) <= z)%Q
   | 64 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 65 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 66 => ((1 # 1) + s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | 67 => (s V_III_hybrid_z
            + (1 # 2) * max0(1 + s V_III_hybrid_gr_info_dref_off64
                             - s V_III_hybrid_sb)
            + (19 # 1) * max0(32 - s V_III_hybrid_sb) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_hybrid =>
    [mkPA Q (fun n z s => ai_III_hybrid n s /\ annot0_III_hybrid n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_hybrid (proc_start P_III_hybrid) s1 (proc_end P_III_hybrid) s2 ->
    (s2 V_III_hybrid_z <= (608 # 1)
                          + (1 # 2) * max0(-1
                                           + s1 V_III_hybrid_gr_info_dref_off64)
                          + (1 # 2) * max0(1
                                           + s1 V_III_hybrid_gr_info_dref_off64))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_hybrid.
Qed.
