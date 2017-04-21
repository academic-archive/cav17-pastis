Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pr_suf_expansion.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pr_suf_expansion_z := 1%positive.
Notation V_pr_suf_expansion__tmp := 2%positive.
Notation V_pr_suf_expansion__tmp1 := 3%positive.
Notation V_pr_suf_expansion_cond := 4%positive.
Notation V_pr_suf_expansion_flent_dref_off18 := 5%positive.
Notation V_pr_suf_expansion_flent_dref_off20 := 6%positive.
Notation V_pr_suf_expansion_flent_dref_off22 := 7%positive.
Notation V_pr_suf_expansion_tlen := 8%positive.
Notation V_pr_suf_expansion_croot := 9%positive.
Notation V_pr_suf_expansion_extra := 10%positive.
Notation V_pr_suf_expansion_flent := 11%positive.
Notation V_pr_suf_expansion_option := 12%positive.
Notation V_pr_suf_expansion_rootword := 13%positive.
Definition Pedges_pr_suf_expansion: list (edge proc) :=
  (EA 1 (AAssign V_pr_suf_expansion_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_pr_suf_expansion__tmp1 (Some (EVar V_pr_suf_expansion_option))) 3)::
  (EA 3 (AAssign V_pr_suf_expansion_tlen None) 4)::(EA 4 (AAssign
  V_pr_suf_expansion_cond
  (Some (EVar V_pr_suf_expansion_flent_dref_off22))) 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_pr_suf_expansion_cond) s) >
  (eval (EVar V_pr_suf_expansion_tlen) s))%Z)) 48)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_pr_suf_expansion_cond) s) <=
  (eval (EVar V_pr_suf_expansion_tlen) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (ESub (EVar V_pr_suf_expansion_tlen)
  (EVar V_pr_suf_expansion_flent_dref_off18)) s) <= (eval (ENum (0))
  s))%Z)) 44)::(EA 8 (AGuard
  (fun s => ((eval (ESub (EVar V_pr_suf_expansion_tlen)
  (EVar V_pr_suf_expansion_flent_dref_off18)) s) > (eval (ENum (0))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 11 (AAssign
  V_pr_suf_expansion_cond (Some (EAdd (EVar V_pr_suf_expansion_cond)
  (ENum (-1))))) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EAdd (EVar V_pr_suf_expansion_cond) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 36)::(EA 13 (AGuard
  (fun s => ((eval (EAdd (EVar V_pr_suf_expansion_cond) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_pr_suf_expansion_flent_dref_off20) s) <>
  (eval (ENum (0)) s))%Z)) 19)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_pr_suf_expansion_flent_dref_off20) s) =
  (eval (ENum (0)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 24)::(EA 19 AWeaken 20)::(EA 20 ANone 22)::
  (EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::
  (EA 24 (AGuard (fun s => ((eval (EVar V_pr_suf_expansion__tmp1) s) =
  (eval (ENum (3)) s))%Z)) 26)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_pr_suf_expansion__tmp1) s) <> (eval (ENum (3))
  s))%Z)) 25)::(EA 25 AWeaken 29)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_pr_suf_expansion__tmp1) s) <> (eval (ENum (4))
  s))%Z)) 31)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_pr_suf_expansion__tmp1) s) = (eval (ENum (4))
  s))%Z)) 30)::(EA 30 AWeaken 33)::(EA 31 AWeaken 32)::(EA 32 ANone 33)::
  (EA 33 (AAssign V_pr_suf_expansion__tmp
  (Some (ESub (EAdd (EVar V_pr_suf_expansion_tlen)
  (EVar V_pr_suf_expansion_flent_dref_off20))
  (EVar V_pr_suf_expansion_flent_dref_off18)))) 34)::(EA 34 ANone 35)::
  (EA 35 AWeaken 52)::(EA 36 AWeaken 37)::(EA 37 ANone 41)::
  (EA 37 ANone 38)::(EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_pr_suf_expansion_z (Some (EAdd (ENum (1))
  (EVar V_pr_suf_expansion_z)))) 11)::(EA 41 (AAssign V_pr_suf_expansion__tmp
  (Some (ENum (0)))) 42)::(EA 42 ANone 43)::(EA 43 AWeaken 52)::
  (EA 44 AWeaken 45)::(EA 45 (AAssign V_pr_suf_expansion__tmp
  (Some (ENum (0)))) 46)::(EA 46 ANone 47)::(EA 47 AWeaken 52)::
  (EA 48 AWeaken 49)::(EA 49 (AAssign V_pr_suf_expansion__tmp
  (Some (ENum (0)))) 50)::(EA 50 ANone 51)::(EA 51 AWeaken 52)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pr_suf_expansion => Pedges_pr_suf_expansion
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pr_suf_expansion => 52
     end)%positive;
  var_global := var_global
}.

Definition ai_pr_suf_expansion (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 3 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0)%Z
   | 4 => (1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 5 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0)%Z
   | 6 => (1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 7 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0)%Z
   | 8 => (1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 9 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 10 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 11 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 12 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 13 => (1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 14 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 15 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 16 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off20 <= 0 /\ -1 * s V_pr_suf_expansion_flent_dref_off20 <= 0)%Z
   | 17 => (-1 * s V_pr_suf_expansion_flent_dref_off20 <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off20 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 18 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off20 <= 0 /\ -1 * s V_pr_suf_expansion_flent_dref_off20 <= 0)%Z
   | 19 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 20 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 21 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 22 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 23 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 24 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 25 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 26 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion__tmp1 + -3 <= 0 /\ -1 * s V_pr_suf_expansion__tmp1 + 3 <= 0)%Z
   | 27 => (-1 * s V_pr_suf_expansion__tmp1 + 3 <= 0 /\ 1 * s V_pr_suf_expansion__tmp1 + -3 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 28 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion__tmp1 + -3 <= 0 /\ -1 * s V_pr_suf_expansion__tmp1 + 3 <= 0)%Z
   | 29 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 30 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion__tmp1 + -4 <= 0 /\ -1 * s V_pr_suf_expansion__tmp1 + 4 <= 0)%Z
   | 31 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 32 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 33 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 34 => (1 * s V_pr_suf_expansion_cond <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 35 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond <= 0)%Z
   | 36 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_cond + 1 <= 0)%Z
   | 37 => (-1 * s V_pr_suf_expansion_cond + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 38 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_cond + 1 <= 0)%Z
   | 39 => (-1 * s V_pr_suf_expansion_cond + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 40 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_cond + 1 <= 0)%Z
   | 41 => (1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_cond + 1 <= 0)%Z
   | 42 => (-1 * s V_pr_suf_expansion_cond + 1 <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion__tmp <= 0 /\ -1 * s V_pr_suf_expansion__tmp <= 0)%Z
   | 43 => (-1 * s V_pr_suf_expansion__tmp <= 0 /\ 1 * s V_pr_suf_expansion__tmp <= 0 /\ 1 * s V_pr_suf_expansion_flent_dref_off18+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ -1 * s V_pr_suf_expansion_cond + 1 <= 0)%Z
   | 44 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ -1 * s V_pr_suf_expansion_flent_dref_off18+ 1 * s V_pr_suf_expansion_tlen <= 0)%Z
   | 45 => (-1 * s V_pr_suf_expansion_flent_dref_off18+ 1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 46 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ -1 * s V_pr_suf_expansion_flent_dref_off18+ 1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion__tmp <= 0 /\ -1 * s V_pr_suf_expansion__tmp <= 0)%Z
   | 47 => (-1 * s V_pr_suf_expansion__tmp <= 0 /\ 1 * s V_pr_suf_expansion__tmp <= 0 /\ -1 * s V_pr_suf_expansion_flent_dref_off18+ 1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_cond+ -1 * s V_pr_suf_expansion_tlen <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 48 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_cond+ 1 * s V_pr_suf_expansion_tlen + 1 <= 0)%Z
   | 49 => (-1 * s V_pr_suf_expansion_cond+ 1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 50 => (-1 * s V_pr_suf_expansion_z <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_cond+ 1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion__tmp <= 0 /\ -1 * s V_pr_suf_expansion__tmp <= 0)%Z
   | 51 => (-1 * s V_pr_suf_expansion__tmp <= 0 /\ 1 * s V_pr_suf_expansion__tmp <= 0 /\ -1 * s V_pr_suf_expansion_cond+ 1 * s V_pr_suf_expansion_tlen + 1 <= 0 /\ 1 * s V_pr_suf_expansion_z <= 0 /\ -1 * s V_pr_suf_expansion_z <= 0)%Z
   | 52 => (-1 * s V_pr_suf_expansion_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pr_suf_expansion (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_pr_suf_expansion_flent_dref_off22) <= z)%Q
   | 2 => (s V_pr_suf_expansion_z
           + max0(s V_pr_suf_expansion_flent_dref_off22) <= z)%Q
   | 3 => (s V_pr_suf_expansion_z
           + max0(s V_pr_suf_expansion_flent_dref_off22) <= z)%Q
   | 4 => (s V_pr_suf_expansion_z
           + max0(s V_pr_suf_expansion_flent_dref_off22) <= z)%Q
   | 5 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 6 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 7 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 8 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 9 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_pr_suf_expansion_cond) (-1
                                                                    + s V_pr_suf_expansion_cond))]
     (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 10 => (s V_pr_suf_expansion_z + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 11 => (s V_pr_suf_expansion_z + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 12 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 13 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 14 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 15 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 16 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 17 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 18 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 19 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 20 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 21 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 22 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 23 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 24 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 25 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 26 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 27 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 28 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 29 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 30 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_pr_suf_expansion_cond) (-1
                                                                    + s V_pr_suf_expansion_cond));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pr_suf_expansion_cond)]
     (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 31 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_pr_suf_expansion_cond) (-1
                                                                    + s V_pr_suf_expansion_cond));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pr_suf_expansion_cond)]
     (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 32 => (s V_pr_suf_expansion_z <= z)%Q
   | 33 => (s V_pr_suf_expansion_z <= z)%Q
   | 34 => (s V_pr_suf_expansion_z <= z)%Q
   | 35 => (s V_pr_suf_expansion_z <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pr_suf_expansion_cond) (1)]
     (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 37 => ((1 # 1) + s V_pr_suf_expansion_z
            + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 38 => ((1 # 1) + s V_pr_suf_expansion_z
            + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 39 => ((1 # 1) + s V_pr_suf_expansion_z
            + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 40 => ((1 # 1) + s V_pr_suf_expansion_z
            + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 41 => ((1 # 1) + s V_pr_suf_expansion_z
            + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 42 => ((1 # 1) + s V_pr_suf_expansion_z
            + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 43 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (-1 + s V_pr_suf_expansion_cond)]
     ((1 # 1) + s V_pr_suf_expansion_z + max0(-1 + s V_pr_suf_expansion_cond) <= z)%Q
   | 44 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 45 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 46 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pr_suf_expansion_cond) (-1
                                                                    + s V_pr_suf_expansion_cond));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pr_suf_expansion_cond)]
     (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 48 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 49 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 50 => (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pr_suf_expansion_cond) (-1
                                                                    + s V_pr_suf_expansion_cond));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pr_suf_expansion_cond)]
     (s V_pr_suf_expansion_z + max0(s V_pr_suf_expansion_cond) <= z)%Q
   | 52 => (s V_pr_suf_expansion_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pr_suf_expansion =>
    [mkPA Q (fun n z s => ai_pr_suf_expansion n s /\ annot0_pr_suf_expansion n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pr_suf_expansion (proc_start P_pr_suf_expansion) s1 (proc_end P_pr_suf_expansion) s2 ->
    (s2 V_pr_suf_expansion_z <= max0(s1 V_pr_suf_expansion_flent_dref_off22))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pr_suf_expansion.
Qed.
