Require Import pasta.Pasta.

Inductive proc: Type :=
  P_encode_mcu_AC_refine.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_encode_mcu_AC_refine_z := 1%positive.
Notation V_encode_mcu_AC_refine_Al := 2%positive.
Notation V_encode_mcu_AC_refine_BR := 3%positive.
Notation V_encode_mcu_AC_refine_EOB := 4%positive.
Notation V_encode_mcu_AC_refine_Se := 5%positive.
Notation V_encode_mcu_AC_refine_cinfo_dref_off272 := 6%positive.
Notation V_encode_mcu_AC_refine_cinfo_dref_off404 := 7%positive.
Notation V_encode_mcu_AC_refine_cinfo_dref_off408 := 8%positive.
Notation V_encode_mcu_AC_refine_cinfo_dref_off416 := 9%positive.
Notation V_encode_mcu_AC_refine_k := 10%positive.
Notation V_encode_mcu_AC_refine_r := 11%positive.
Notation V_encode_mcu_AC_refine_temp := 12%positive.
Notation V_encode_mcu_AC_refine_MCU_data := 13%positive.
Notation V_encode_mcu_AC_refine_cinfo := 14%positive.
Definition Pedges_encode_mcu_AC_refine: list (edge proc) :=
  (EA 1 (AAssign V_encode_mcu_AC_refine_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_encode_mcu_AC_refine_BR) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_encode_mcu_AC_refine_Se
  (Some (EVar V_encode_mcu_AC_refine_cinfo_dref_off408))) 5)::(EA 5 (AAssign
  V_encode_mcu_AC_refine_Al
  (Some (EVar V_encode_mcu_AC_refine_cinfo_dref_off416))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_cinfo_dref_off272) s) <>
  (eval (ENum (0)) s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_cinfo_dref_off272) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 13)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 10 ANone 12)::(EA 11 ANone 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_encode_mcu_AC_refine_EOB (Some (ENum (0)))) 14)::
  (EA 14 (AAssign V_encode_mcu_AC_refine_k
  (Some (EVar V_encode_mcu_AC_refine_cinfo_dref_off404))) 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_k) s) <=
  (eval (EVar V_encode_mcu_AC_refine_Se) s))%Z)) 82)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_k) s) >
  (eval (EVar V_encode_mcu_AC_refine_Se) s))%Z)) 18)::(EA 18 AWeaken 19)::
  (EA 19 (AAssign V_encode_mcu_AC_refine_r (Some (ENum (0)))) 20)::
  (EA 20 (AAssign V_encode_mcu_AC_refine_BR (Some (ENum (0)))) 21)::
  (EA 21 (AAssign V_encode_mcu_AC_refine_k
  (Some (EVar V_encode_mcu_AC_refine_cinfo_dref_off404))) 22)::
  (EA 22 ANone 23)::(EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_k) s) <=
  (eval (EVar V_encode_mcu_AC_refine_Se) s))%Z)) 46)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_k) s) >
  (eval (EVar V_encode_mcu_AC_refine_Se) s))%Z)) 25)::(EA 25 AWeaken 26)::
  (EA 26 (AGuard (fun s => ((eval (EVar V_encode_mcu_AC_refine_r) s) >
  (eval (ENum (0)) s))%Z)) 31)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_r) s) <= (eval (ENum (0))
  s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_BR) s) > (eval (ENum (0))
  s))%Z)) 30)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_BR) s) <= (eval (ENum (0))
  s))%Z)) 29)::(EA 29 AWeaken 38)::(EA 30 AWeaken 32)::(EA 31 AWeaken 32)::
  (EA 32 ANone 35)::(EA 32 ANone 33)::(EA 33 AWeaken 34)::(EA 34 ANone 35)::
  (EA 34 ANone 36)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 AWeaken 38)::
  (EA 38 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_cinfo_dref_off272) s) <>
  (eval (ENum (0)) s))%Z)) 40)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_cinfo_dref_off272) s) =
  (eval (ENum (0)) s))%Z)) 39)::(EA 39 AWeaken 45)::(EA 40 AWeaken 41)::
  (EA 41 ANone 42)::(EA 41 ANone 43)::(EA 42 ANone 43)::(EA 43 ANone 44)::
  (EA 44 AWeaken 45)::(EA 46 AWeaken 47)::(EA 47 (AAssign
  V_encode_mcu_AC_refine_temp None) 48)::(EA 48 AWeaken 49)::
  (EA 49 ANone 75)::(EA 49 ANone 50)::(EA 50 ANone 51)::(EA 51 AWeaken 52)::
  (EA 52 (AGuard (fun s => ((eval (EVar V_encode_mcu_AC_refine_r) s) >
  (eval (ENum (15)) s))%Z)) 54)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_r) s) <= (eval (ENum (15))
  s))%Z)) 53)::(EA 53 AWeaken 59)::(EA 54 AWeaken 55)::(EA 55 ANone 56)::
  (EA 56 AWeaken 57)::(EA 57 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_k) s) <=
  (eval (EVar V_encode_mcu_AC_refine_EOB) s))%Z)) 68)::(EA 57 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_k) s) >
  (eval (EVar V_encode_mcu_AC_refine_EOB) s))%Z)) 58)::(EA 58 AWeaken 59)::
  (EA 59 (AGuard (fun s => ((eval (EVar V_encode_mcu_AC_refine_temp) s) >
  (eval (ENum (1)) s))%Z)) 65)::(EA 59 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_temp) s) <= (eval (ENum (1))
  s))%Z)) 60)::(EA 60 AWeaken 61)::(EA 61 (AAssign
  V_encode_mcu_AC_refine_temp None) 62)::(EA 62 (AAssign
  V_encode_mcu_AC_refine_BR (Some (ENum (0)))) 63)::(EA 63 (AAssign
  V_encode_mcu_AC_refine_r (Some (ENum (0)))) 64)::(EA 64 ANone 77)::
  (EA 65 AWeaken 66)::(EA 66 (AAssign V_encode_mcu_AC_refine_BR
  (Some (EAdd (EVar V_encode_mcu_AC_refine_BR) (ENum (1))))) 67)::
  (EA 67 ANone 77)::(EA 68 AWeaken 69)::(EA 69 (AAssign
  V_encode_mcu_AC_refine_r (Some (ESub (EVar V_encode_mcu_AC_refine_r)
  (ENum (16))))) 70)::(EA 70 (AAssign V_encode_mcu_AC_refine_BR
  (Some (ENum (0)))) 71)::(EA 71 ANone 72)::(EA 72 ANone 73)::(EA 73 (AAssign
  V_encode_mcu_AC_refine_z (Some (EAdd (ENum (1))
  (EVar V_encode_mcu_AC_refine_z)))) 74)::(EA 74 AWeaken 52)::(EA 75 (AAssign
  V_encode_mcu_AC_refine_r (Some (EAdd (EVar V_encode_mcu_AC_refine_r)
  (ENum (1))))) 76)::(EA 76 ANone 77)::(EA 77 (AAssign
  V_encode_mcu_AC_refine_k (Some (EAdd (EVar V_encode_mcu_AC_refine_k)
  (ENum (1))))) 78)::(EA 78 ANone 79)::(EA 79 ANone 80)::(EA 80 (AAssign
  V_encode_mcu_AC_refine_z (Some (EAdd (ENum (1))
  (EVar V_encode_mcu_AC_refine_z)))) 81)::(EA 81 AWeaken 24)::
  (EA 82 AWeaken 83)::(EA 83 (AAssign V_encode_mcu_AC_refine_temp None) 84)::
  (EA 84 AWeaken 85)::(EA 85 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_temp) s) < (eval (ENum (0))
  s))%Z)) 87)::(EA 85 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_temp) s) >= (eval (ENum (0))
  s))%Z)) 86)::(EA 86 AWeaken 90)::(EA 87 AWeaken 88)::(EA 88 (AAssign
  V_encode_mcu_AC_refine_temp (Some (ESub (ENum (0))
  (EVar V_encode_mcu_AC_refine_temp)))) 89)::(EA 89 ANone 90)::
  (EA 90 (AAssign V_encode_mcu_AC_refine_temp None) 91)::(EA 91 AWeaken 92)::
  (EA 92 (AGuard (fun s => ((eval (EVar V_encode_mcu_AC_refine_temp) s) =
  (eval (ENum (1)) s))%Z)) 94)::(EA 92 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_AC_refine_temp) s) <> (eval (ENum (1))
  s))%Z)) 93)::(EA 93 AWeaken 97)::(EA 94 AWeaken 95)::(EA 95 (AAssign
  V_encode_mcu_AC_refine_EOB (Some (EVar V_encode_mcu_AC_refine_k))) 96)::
  (EA 96 ANone 97)::(EA 97 ANone 98)::(EA 98 (AAssign
  V_encode_mcu_AC_refine_k (Some (EAdd (EVar V_encode_mcu_AC_refine_k)
  (ENum (1))))) 99)::(EA 99 ANone 100)::(EA 100 ANone 101)::(EA 101 (AAssign
  V_encode_mcu_AC_refine_z (Some (EAdd (ENum (1))
  (EVar V_encode_mcu_AC_refine_z)))) 102)::(EA 102 AWeaken 17)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_encode_mcu_AC_refine => Pedges_encode_mcu_AC_refine
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_encode_mcu_AC_refine => 45
     end)%positive;
  var_global := var_global
}.

Definition ai_encode_mcu_AC_refine (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 3 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 4 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 5 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 6 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 7 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 8 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_cinfo_dref_off272 <= 0 /\ -1 * s V_encode_mcu_AC_refine_cinfo_dref_off272 <= 0)%Z
   | 9 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 10 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 11 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 12 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 13 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 14 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_EOB <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB <= 0)%Z
   | 15 => (-1 * s V_encode_mcu_AC_refine_EOB <= 0 /\ 1 * s V_encode_mcu_AC_refine_EOB <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 16 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_EOB <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB <= 0)%Z
   | 17 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 18 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0)%Z
   | 19 => (1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 20 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 21 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 22 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 23 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 24 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 25 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0)%Z
   | 26 => (1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 27 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 28 => (1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 29 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 30 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR + 1 <= 0)%Z
   | 31 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 1 <= 0)%Z
   | 32 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 33 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 34 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 35 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 36 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 37 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 38 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 39 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_cinfo_dref_off272 <= 0 /\ -1 * s V_encode_mcu_AC_refine_cinfo_dref_off272 <= 0)%Z
   | 40 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 41 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 42 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 43 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 44 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 45 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_Se+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 46 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 47 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 48 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 49 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 50 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 51 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 52 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 53 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_r + -15 <= 0)%Z
   | 54 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 16 <= 0)%Z
   | 55 => (-1 * s V_encode_mcu_AC_refine_r + 16 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 56 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 16 <= 0)%Z
   | 57 => (-1 * s V_encode_mcu_AC_refine_r + 16 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 58 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 16 <= 0 /\ 1 * s V_encode_mcu_AC_refine_EOB+ -1 * s V_encode_mcu_AC_refine_k + 1 <= 0)%Z
   | 59 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 60 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ 1 * s V_encode_mcu_AC_refine_temp + -1 <= 0)%Z
   | 61 => (1 * s V_encode_mcu_AC_refine_temp + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 62 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 63 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 64 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ 1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 65 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_temp + 2 <= 0)%Z
   | 66 => (-1 * s V_encode_mcu_AC_refine_temp + 2 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 67 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_temp + 2 <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR + 1 <= 0)%Z
   | 68 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 16 <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 69 => (-1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 16 <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 70 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 71 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 72 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0)%Z
   | 73 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 74 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ 1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_EOB+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_z + 1 <= 0)%Z
   | 75 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 76 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r + 1 <= 0)%Z
   | 77 => (-1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 78 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0)%Z
   | 79 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 80 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0)%Z
   | 81 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_r <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z + 1 <= 0)%Z
   | 82 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 83 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 84 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 85 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 86 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_temp <= 0)%Z
   | 87 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ 1 * s V_encode_mcu_AC_refine_temp + 1 <= 0)%Z
   | 88 => (1 * s V_encode_mcu_AC_refine_temp + 1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 89 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_temp + 1 <= 0)%Z
   | 90 => (-1 * s V_encode_mcu_AC_refine_temp <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 91 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 92 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 93 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 94 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ 1 * s V_encode_mcu_AC_refine_temp + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_temp + 1 <= 0)%Z
   | 95 => (-1 * s V_encode_mcu_AC_refine_temp + 1 <= 0 /\ 1 * s V_encode_mcu_AC_refine_temp + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 96 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ 1 * s V_encode_mcu_AC_refine_temp + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_temp + 1 <= 0 /\ 1 * s V_encode_mcu_AC_refine_EOB+ -1 * s V_encode_mcu_AC_refine_Se <= 0)%Z
   | 97 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0)%Z
   | 98 => (-1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k <= 0)%Z
   | 99 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0)%Z
   | 100 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z <= 0)%Z
   | 101 => (-1 * s V_encode_mcu_AC_refine_z <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0)%Z
   | 102 => (-1 * s V_encode_mcu_AC_refine_Se+ 1 * s V_encode_mcu_AC_refine_k + -1 <= 0 /\ -1 * s V_encode_mcu_AC_refine_BR <= 0 /\ -1 * s V_encode_mcu_AC_refine_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_encode_mcu_AC_refine (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((33 # 16) * max0(1 - s V_encode_mcu_AC_refine_cinfo_dref_off404
                            + s V_encode_mcu_AC_refine_cinfo_dref_off408) <= z)%Q
   | 2 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 - s V_encode_mcu_AC_refine_cinfo_dref_off404
                              + s V_encode_mcu_AC_refine_cinfo_dref_off408) <= z)%Q
   | 3 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 - s V_encode_mcu_AC_refine_cinfo_dref_off404
                              + s V_encode_mcu_AC_refine_cinfo_dref_off408) <= z)%Q
   | 4 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 - s V_encode_mcu_AC_refine_cinfo_dref_off404
                              + s V_encode_mcu_AC_refine_cinfo_dref_off408) <= z)%Q
   | 5 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                              - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 6 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                              - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 7 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                              - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 8 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                              - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 9 => (s V_encode_mcu_AC_refine_z
           + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                              - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 10 => (s V_encode_mcu_AC_refine_z
            + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 11 => (s V_encode_mcu_AC_refine_z
            + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 12 => (s V_encode_mcu_AC_refine_z
            + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 13 => (s V_encode_mcu_AC_refine_z
            + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 14 => (s V_encode_mcu_AC_refine_z
            + (33 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404) <= z)%Q
   | 15 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_encode_mcu_AC_refine_z) (0))) (F_max0_ge_0 (s V_encode_mcu_AC_refine_z))]
     (s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_cinfo_dref_off404)
      + max0(1 + s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 17 => ((17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                             - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_encode_mcu_AC_refine_Se
                                             - s V_encode_mcu_AC_refine_k) (s V_encode_mcu_AC_refine_Se
                                                                    - s V_encode_mcu_AC_refine_k));
      (*-1 0*) F_max0_ge_0 (s V_encode_mcu_AC_refine_Se
                            - s V_encode_mcu_AC_refine_k)]
     ((17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                       - s V_encode_mcu_AC_refine_cinfo_dref_off404)
      + max0(1 + s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k)
      + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 19 => ((17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                             - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 20 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 21 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 22 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_encode_mcu_AC_refine_z)) (F_check_ge (s V_encode_mcu_AC_refine_z) (0))]
     ((1 # 16) * s V_encode_mcu_AC_refine_r
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k)
      + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 24 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 25 => hints
     [(*0 1.0625*) F_max0_monotonic (F_check_ge (1
                                                 + s V_encode_mcu_AC_refine_Se
                                                 - s V_encode_mcu_AC_refine_k) (s V_encode_mcu_AC_refine_Se
                                                                    - s V_encode_mcu_AC_refine_k))]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 26 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 27 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 28 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 29 => hints
     [(*-1.0625 0*) F_max0_ge_0 (s V_encode_mcu_AC_refine_Se
                                 - s V_encode_mcu_AC_refine_k)]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 30 => hints
     [(*-1.0625 0*) F_max0_ge_0 (s V_encode_mcu_AC_refine_Se
                                 - s V_encode_mcu_AC_refine_k)]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 31 => hints
     [(*-1.0625 0*) F_max0_ge_0 (s V_encode_mcu_AC_refine_Se
                                 - s V_encode_mcu_AC_refine_k)]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 32 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 33 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 34 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 35 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 36 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 37 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 38 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 39 => hints
     [(*-0.0625 0*) F_max0_monotonic (F_check_ge (s V_encode_mcu_AC_refine_r) (-16
                                                                    + s V_encode_mcu_AC_refine_r));
      (*-0.0625 0*) F_max0_ge_0 (-16 + s V_encode_mcu_AC_refine_r);
      (*-0.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_encode_mcu_AC_refine_r) (0))) (F_max0_ge_0 (s V_encode_mcu_AC_refine_r))]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z <= z)%Q
   | 40 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 41 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 42 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 43 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z <= z)%Q
   | 44 => hints
     [(*-0.0625 0*) F_max0_monotonic (F_check_ge (s V_encode_mcu_AC_refine_r) (-16
                                                                    + s V_encode_mcu_AC_refine_r));
      (*-0.0625 0*) F_max0_ge_0 (-16 + s V_encode_mcu_AC_refine_r);
      (*-0.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_encode_mcu_AC_refine_r) (0))) (F_max0_ge_0 (s V_encode_mcu_AC_refine_r))]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z <= z)%Q
   | 45 => (s V_encode_mcu_AC_refine_z <= z)%Q
   | 46 => hints
     [(*-1.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_encode_mcu_AC_refine_Se
                                                                    - 
                                                                    s V_encode_mcu_AC_refine_k) (0))) (F_max0_ge_0 (s V_encode_mcu_AC_refine_Se
                                                                    - s V_encode_mcu_AC_refine_k))]
     ((1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 47 => (-(17 # 16) * s V_encode_mcu_AC_refine_Se
            + (17 # 16) * s V_encode_mcu_AC_refine_k
            + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k)
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 48 => hints
     [(*-1.0625 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                        + s V_encode_mcu_AC_refine_Se
                                                        - s V_encode_mcu_AC_refine_k)) (F_check_ge (1
                                                                    + s V_encode_mcu_AC_refine_Se
                                                                    - s V_encode_mcu_AC_refine_k) (0))]
     (-(17 # 16) * s V_encode_mcu_AC_refine_Se
      + (17 # 16) * s V_encode_mcu_AC_refine_k
      + (1 # 16) * s V_encode_mcu_AC_refine_r + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k)
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 49 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 50 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 51 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 52 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 53 => hints
     [(*-0.0625 0*) F_one]
     ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
      + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 54 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 55 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 56 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 57 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 58 => hints
     [(*-0.0625 0*) F_one]
     ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
      + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 59 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 60 => hints
     [(*-0.0625 0*) F_max0_monotonic (F_check_ge (s V_encode_mcu_AC_refine_r) (-16
                                                                    + s V_encode_mcu_AC_refine_r));
      (*-0.0625 0*) F_max0_ge_0 (-16 + s V_encode_mcu_AC_refine_r);
      (*-0.0625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_encode_mcu_AC_refine_r) (0))) (F_max0_ge_0 (s V_encode_mcu_AC_refine_r))]
     ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
      + s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 61 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 62 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 63 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 64 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 65 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 66 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 67 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 68 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 69 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 70 => ((33 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 71 => ((33 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 72 => ((33 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 73 => ((33 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 74 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 75 => ((17 # 16) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 76 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 77 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 78 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 79 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 80 => ((1 # 1) + (1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 81 => ((1 # 16) * s V_encode_mcu_AC_refine_r
            + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 82 => ((17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                             - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 83 => ((17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                             - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k)
            + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 84 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_encode_mcu_AC_refine_z)) (F_check_ge (s V_encode_mcu_AC_refine_z) (0))]
     ((17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                       - s V_encode_mcu_AC_refine_cinfo_dref_off404)
      + max0(1 + s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k)
      + max0(s V_encode_mcu_AC_refine_z) <= z)%Q
   | 85 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 86 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 87 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 88 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 89 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 90 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 91 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 92 => (s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 93 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_encode_mcu_AC_refine_Se
                                      - s V_encode_mcu_AC_refine_k) (1)]
     (s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_cinfo_dref_off404)
      + max0(1 + s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 94 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_encode_mcu_AC_refine_Se
                                       - s V_encode_mcu_AC_refine_k) (1)]
     (s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_cinfo_dref_off404)
      + max0(1 + s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 95 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 96 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 97 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 98 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 99 => ((1 # 1) + s V_encode_mcu_AC_refine_z
            + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                               - s V_encode_mcu_AC_refine_cinfo_dref_off404)
            + max0(1 + s V_encode_mcu_AC_refine_Se
                   - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 100 => ((1 # 1) + s V_encode_mcu_AC_refine_z
             + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                                - s V_encode_mcu_AC_refine_cinfo_dref_off404)
             + max0(1 + s V_encode_mcu_AC_refine_Se
                    - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 101 => ((1 # 1) + s V_encode_mcu_AC_refine_z
             + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                                - s V_encode_mcu_AC_refine_cinfo_dref_off404)
             + max0(1 + s V_encode_mcu_AC_refine_Se
                    - s V_encode_mcu_AC_refine_k) <= z)%Q
   | 102 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_encode_mcu_AC_refine_z) (0))) (F_max0_ge_0 (s V_encode_mcu_AC_refine_z))]
     (s V_encode_mcu_AC_refine_z
      + (17 # 16) * max0(1 + s V_encode_mcu_AC_refine_Se
                         - s V_encode_mcu_AC_refine_cinfo_dref_off404)
      + max0(1 + s V_encode_mcu_AC_refine_Se - s V_encode_mcu_AC_refine_k) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_encode_mcu_AC_refine =>
    [mkPA Q (fun n z s => ai_encode_mcu_AC_refine n s /\ annot0_encode_mcu_AC_refine n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_encode_mcu_AC_refine (proc_start P_encode_mcu_AC_refine) s1 (proc_end P_encode_mcu_AC_refine) s2 ->
    (s2 V_encode_mcu_AC_refine_z <= (33 # 16) * max0(1
                                                     - s1 V_encode_mcu_AC_refine_cinfo_dref_off404
                                                     + s1 V_encode_mcu_AC_refine_cinfo_dref_off408))%Q.
Proof.
  prove_bound ipa admissible_ipa P_encode_mcu_AC_refine.
Qed.
