Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gsm_div.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gsm_div_z := 1%positive.
Notation V_gsm_div_L_denum := 2%positive.
Notation V_gsm_div_L_num := 3%positive.
Notation V_gsm_div__tmp := 4%positive.
Notation V_gsm_div__tmp1 := 5%positive.
Notation V_gsm_div__tmp2 := 6%positive.
Notation V_gsm_div_div := 7%positive.
Notation V_gsm_div_k := 8%positive.
Notation V_gsm_div_denum := 9%positive.
Notation V_gsm_div_num := 10%positive.
Definition Pedges_gsm_div: list (edge proc) :=
  (EA 1 (AAssign V_gsm_div_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gsm_div__tmp (Some (EVar V_gsm_div_num))) 3)::(EA 3 (AAssign
  V_gsm_div__tmp1 (Some (EVar V_gsm_div_denum))) 4)::(EA 4 (AAssign
  V_gsm_div_L_num (Some (EVar V_gsm_div__tmp))) 5)::(EA 5 (AAssign
  V_gsm_div_L_denum (Some (EVar V_gsm_div__tmp1))) 6)::(EA 6 (AAssign
  V_gsm_div_div (Some (ENum (0)))) 7)::(EA 7 (AAssign V_gsm_div_k
  (Some (ENum (15)))) 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_gsm_div__tmp) s) >= (eval (ENum (0)) s))%Z)) 11)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_gsm_div__tmp) s) < (eval (ENum (0))
  s))%Z)) 10)::(EA 10 AWeaken 14)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_gsm_div__tmp1) s) >= (eval (EVar V_gsm_div__tmp)
  s))%Z)) 16)::(EA 12 (AGuard (fun s => ((eval (EVar V_gsm_div__tmp1) s) <
  (eval (EVar V_gsm_div__tmp) s))%Z)) 13)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 AWeaken 46)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_gsm_div__tmp) s) = (eval (ENum (0)) s))%Z)) 42)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_gsm_div__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::
  (EA 22 (AAssign V_gsm_div_k (Some (EAdd (EVar V_gsm_div_k)
  (ENum (-1))))) 23)::(EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_gsm_div_k) s) <> (eval (ENum (0)) s))%Z)) 29)::
  (EA 24 (AGuard (fun s => ((eval (EVar V_gsm_div_k) s) = (eval (ENum (0))
  s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 (AAssign V_gsm_div__tmp2
  (Some (EVar V_gsm_div_div))) 27)::(EA 27 ANone 28)::(EA 28 AWeaken 46)::
  (EA 29 AWeaken 30)::(EA 30 (AAssign V_gsm_div_div None) 31)::
  (EA 31 (AAssign V_gsm_div_L_num None) 32)::(EA 32 AWeaken 33)::
  (EA 33 (AGuard (fun s => ((eval (EVar V_gsm_div_L_num) s) >=
  (eval (EVar V_gsm_div_L_denum) s))%Z)) 35)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_gsm_div_L_num) s) < (eval (EVar V_gsm_div_L_denum)
  s))%Z)) 34)::(EA 34 AWeaken 39)::(EA 35 AWeaken 36)::(EA 36 (AAssign
  V_gsm_div_L_num (Some (ESub (EVar V_gsm_div_L_num)
  (EVar V_gsm_div_L_denum)))) 37)::(EA 37 (AAssign V_gsm_div_div
  (Some (EAdd (EVar V_gsm_div_div) (ENum (1))))) 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 ANone 41)::(EA 41 (AAssign V_gsm_div_z
  (Some (EAdd (ENum (1)) (EVar V_gsm_div_z)))) 22)::(EA 42 AWeaken 43)::
  (EA 43 (AAssign V_gsm_div__tmp2 (Some (ENum (0)))) 44)::(EA 44 ANone 45)::
  (EA 45 AWeaken 46)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gsm_div => Pedges_gsm_div
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gsm_div => 46
     end)%positive;
  var_global := var_global
}.

Definition ai_gsm_div (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0)%Z
   | 3 => (-1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0)%Z
   | 4 => (1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0)%Z
   | 5 => (-1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0)%Z
   | 6 => (1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0)%Z
   | 7 => (-1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 8 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0)%Z
   | 9 => (-1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 10 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div__tmp + 1 <= 0)%Z
   | 11 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ -1 * s V_gsm_div__tmp <= 0)%Z
   | 12 => (-1 * s V_gsm_div__tmp <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 13 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ -1 * s V_gsm_div__tmp+ 1 * s V_gsm_div__tmp1 + 1 <= 0)%Z
   | 14 => (-1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 15 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0)%Z
   | 16 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 17 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 18 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 19 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 20 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 22 => (-1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 23 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 24 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 25 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k <= 0 /\ -1 * s V_gsm_div_k <= 0)%Z
   | 26 => (-1 * s V_gsm_div_k <= 0 /\ 1 * s V_gsm_div_k <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 27 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k <= 0 /\ -1 * s V_gsm_div_k <= 0)%Z
   | 28 => (-1 * s V_gsm_div_k <= 0 /\ 1 * s V_gsm_div_k <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 29 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 30 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 31 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 32 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 33 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 34 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div_L_denum+ 1 * s V_gsm_div_L_num + 1 <= 0)%Z
   | 35 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ 1 * s V_gsm_div_L_denum+ -1 * s V_gsm_div_L_num <= 0)%Z
   | 36 => (1 * s V_gsm_div_L_denum+ -1 * s V_gsm_div_L_num <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 37 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div_L_num <= 0)%Z
   | 38 => (-1 * s V_gsm_div_L_num <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 39 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 40 => (1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -14 <= 0)%Z
   | 41 => (1 * s V_gsm_div_k + -14 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div__tmp + 1 <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0)%Z
   | 42 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ 1 * s V_gsm_div__tmp <= 0)%Z
   | 43 => (1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 44 => (-1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ 1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp2 <= 0 /\ -1 * s V_gsm_div__tmp2 <= 0)%Z
   | 45 => (-1 * s V_gsm_div__tmp2 <= 0 /\ 1 * s V_gsm_div__tmp2 <= 0 /\ 1 * s V_gsm_div__tmp <= 0 /\ 1 * s V_gsm_div__tmp+ -1 * s V_gsm_div__tmp1 <= 0 /\ -1 * s V_gsm_div__tmp <= 0 /\ -1 * s V_gsm_div_k + 15 <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_div <= 0 /\ -1 * s V_gsm_div_div <= 0)%Z
   | 46 => (-1 * s V_gsm_div_k <= 0 /\ -1 * s V_gsm_div_z <= 0 /\ 1 * s V_gsm_div_k + -15 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gsm_div (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((14 # 1) <= z)%Q
   | 2 => ((14 # 1) + s V_gsm_div_z <= z)%Q
   | 3 => ((14 # 1) + s V_gsm_div_z <= z)%Q
   | 4 => ((14 # 1) + s V_gsm_div_z <= z)%Q
   | 5 => ((14 # 1) + s V_gsm_div_z <= z)%Q
   | 6 => ((14 # 1) + s V_gsm_div_z <= z)%Q
   | 7 => ((14 # 1) + s V_gsm_div_z <= z)%Q
   | 8 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
           + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 9 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
           + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 10 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 11 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 12 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 13 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 14 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 15 => hints
     [(*-0.933333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gsm_div_k)) (F_check_ge (0) (0));
      (*-0.933333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gsm_div_k) (0))) (F_max0_ge_0 (s V_gsm_div_k));
      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-15 + s V_gsm_div_k)) (F_check_ge (0) (0))]
     ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
      + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 16 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 17 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 18 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 19 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 20 => hints
     [(*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_arg (-15
                                                           + s V_gsm_div_k)) (F_check_ge (-15
                                                                    + s V_gsm_div_k) (0))]
     ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
      + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 21 => (-(1 # 1) + s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 22 => (-(1 # 1) + s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 23 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 24 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 25 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 26 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 27 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 28 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gsm_div_k)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gsm_div_k) (0))) (F_max0_ge_0 (s V_gsm_div_k))]
     (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 29 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 30 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 31 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 32 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 33 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 34 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 35 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 36 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 37 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 38 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 39 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 40 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 41 => (s V_gsm_div_k + s V_gsm_div_z <= z)%Q
   | 42 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 43 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 44 => ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
            + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 45 => hints
     [(*-0.933333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gsm_div_k)) (F_check_ge (0) (0));
      (*-0.933333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gsm_div_k) (0))) (F_max0_ge_0 (s V_gsm_div_k));
      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-15 + s V_gsm_div_k)) (F_check_ge (0) (0))]
     ((14 # 15) * s V_gsm_div_k + s V_gsm_div_z
      + (1 # 15) * max0(-15 + s V_gsm_div_k) <= z)%Q
   | 46 => (s V_gsm_div_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gsm_div =>
    [mkPA Q (fun n z s => ai_gsm_div n s /\ annot0_gsm_div n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gsm_div (proc_start P_gsm_div) s1 (proc_end P_gsm_div) s2 ->
    (s2 V_gsm_div_z <= (14 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gsm_div.
Qed.
