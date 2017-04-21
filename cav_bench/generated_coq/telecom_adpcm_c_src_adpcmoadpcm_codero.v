Require Import pasta.Pasta.

Inductive proc: Type :=
  P_adpcm_coder.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_adpcm_coder_z := 1%positive.
Notation V_adpcm_coder__tmp := 2%positive.
Notation V_adpcm_coder_bufferstep := 3%positive.
Notation V_adpcm_coder_delta := 4%positive.
Notation V_adpcm_coder_diff := 5%positive.
Notation V_adpcm_coder_index := 6%positive.
Notation V_adpcm_coder_outputbuffer := 7%positive.
Notation V_adpcm_coder_sign := 8%positive.
Notation V_adpcm_coder_state_dref_off0 := 9%positive.
Notation V_adpcm_coder_state_dref_off2 := 10%positive.
Notation V_adpcm_coder_step := 11%positive.
Notation V_adpcm_coder_val := 12%positive.
Notation V_adpcm_coder_valpred := 13%positive.
Notation V_adpcm_coder_vpdiff := 14%positive.
Notation V_adpcm_coder_indata := 15%positive.
Notation V_adpcm_coder_len := 16%positive.
Notation V_adpcm_coder_outdata := 17%positive.
Notation V_adpcm_coder_state := 18%positive.
Definition Pedges_adpcm_coder: list (edge proc) :=
  (EA 1 (AAssign V_adpcm_coder_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_adpcm_coder__tmp (Some (EVar V_adpcm_coder_len))) 3)::(EA 3 (AAssign
  V_adpcm_coder_outputbuffer (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_adpcm_coder_valpred (Some (EVar V_adpcm_coder_state_dref_off0))) 5)::
  (EA 5 (AAssign V_adpcm_coder_index
  (Some (EVar V_adpcm_coder_state_dref_off2))) 6)::(EA 6 (AAssign
  V_adpcm_coder_step None) 7)::(EA 7 (AAssign V_adpcm_coder_bufferstep
  (Some (ENum (1)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder__tmp) s) > (eval (ENum (0))
  s))%Z)) 20)::(EA 10 (AGuard (fun s => ((eval (EVar V_adpcm_coder__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_bufferstep) s) <> (eval (ENum (0))
  s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_bufferstep) s) = (eval (ENum (0))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 16)::(EA 15 AWeaken 16)::
  (EA 16 (AAssign V_adpcm_coder_state_dref_off0
  (Some (EVar V_adpcm_coder_valpred))) 17)::(EA 17 (AAssign
  V_adpcm_coder_state_dref_off2 (Some (EVar V_adpcm_coder_index))) 18)::
  (EA 18 AWeaken 19)::(EA 20 AWeaken 21)::(EA 21 (AAssign V_adpcm_coder_val
  None) 22)::(EA 22 (AAssign V_adpcm_coder_diff
  (Some (ESub (EVar V_adpcm_coder_val) (EVar V_adpcm_coder_valpred)))) 23)::
  (EA 23 (AAssign V_adpcm_coder_sign None) 24)::(EA 24 AWeaken 25)::
  (EA 25 (AGuard (fun s => ((eval (EVar V_adpcm_coder_sign) s) <>
  (eval (ENum (0)) s))%Z)) 27)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_sign) s) = (eval (ENum (0))
  s))%Z)) 26)::(EA 26 AWeaken 30)::(EA 27 AWeaken 28)::(EA 28 (AAssign
  V_adpcm_coder_diff (Some (ESub (ENum (0))
  (EVar V_adpcm_coder_diff)))) 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_adpcm_coder_delta (Some (ENum (0)))) 31)::(EA 31 (AAssign
  V_adpcm_coder_vpdiff None) 32)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_diff) s) >=
  (eval (EVar V_adpcm_coder_step) s))%Z)) 35)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_diff) s) <
  (eval (EVar V_adpcm_coder_step) s))%Z)) 34)::(EA 34 AWeaken 40)::
  (EA 35 AWeaken 36)::(EA 36 (AAssign V_adpcm_coder_delta
  (Some (ENum (4)))) 37)::(EA 37 (AAssign V_adpcm_coder_diff
  (Some (ESub (EVar V_adpcm_coder_diff) (EVar V_adpcm_coder_step)))) 38)::
  (EA 38 (AAssign V_adpcm_coder_vpdiff
  (Some (EAdd (EVar V_adpcm_coder_vpdiff) (EVar V_adpcm_coder_step)))) 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_adpcm_coder_step None) 41)::
  (EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_diff) s) >=
  (eval (EVar V_adpcm_coder_step) s))%Z)) 44)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_diff) s) <
  (eval (EVar V_adpcm_coder_step) s))%Z)) 43)::(EA 43 AWeaken 49)::
  (EA 44 AWeaken 45)::(EA 45 (AAssign V_adpcm_coder_delta None) 46)::
  (EA 46 (AAssign V_adpcm_coder_diff (Some (ESub (EVar V_adpcm_coder_diff)
  (EVar V_adpcm_coder_step)))) 47)::(EA 47 (AAssign V_adpcm_coder_vpdiff
  (Some (EAdd (EVar V_adpcm_coder_vpdiff) (EVar V_adpcm_coder_step)))) 48)::
  (EA 48 ANone 49)::(EA 49 (AAssign V_adpcm_coder_step None) 50)::
  (EA 50 AWeaken 51)::(EA 51 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_diff) s) >=
  (eval (EVar V_adpcm_coder_step) s))%Z)) 53)::(EA 51 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_diff) s) <
  (eval (EVar V_adpcm_coder_step) s))%Z)) 52)::(EA 52 AWeaken 58)::
  (EA 53 AWeaken 54)::(EA 54 (AAssign V_adpcm_coder_delta None) 55)::
  (EA 55 (AAssign V_adpcm_coder_vpdiff
  (Some (EAdd (EVar V_adpcm_coder_vpdiff) (EVar V_adpcm_coder_step)))) 56)::
  (EA 56 ANone 57)::(EA 57 AWeaken 58)::(EA 58 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_sign) s) <> (eval (ENum (0))
  s))%Z)) 63)::(EA 58 (AGuard (fun s => ((eval (EVar V_adpcm_coder_sign) s) =
  (eval (ENum (0)) s))%Z)) 59)::(EA 59 AWeaken 60)::(EA 60 (AAssign
  V_adpcm_coder_valpred (Some (EAdd (EVar V_adpcm_coder_valpred)
  (EVar V_adpcm_coder_vpdiff)))) 61)::(EA 61 ANone 62)::(EA 62 AWeaken 67)::
  (EA 63 AWeaken 64)::(EA 64 (AAssign V_adpcm_coder_valpred
  (Some (ESub (EVar V_adpcm_coder_valpred)
  (EVar V_adpcm_coder_vpdiff)))) 65)::(EA 65 ANone 66)::(EA 66 AWeaken 67)::
  (EA 67 ANone 73)::(EA 67 ANone 68)::(EA 68 AWeaken 69)::(EA 69 ANone 70)::
  (EA 69 ANone 72)::(EA 70 (AAssign V_adpcm_coder_valpred None) 71)::
  (EA 71 ANone 72)::(EA 72 ANone 75)::(EA 73 (AAssign V_adpcm_coder_valpred
  None) 74)::(EA 74 ANone 75)::(EA 75 (AAssign V_adpcm_coder_delta
  None) 76)::(EA 76 (AAssign V_adpcm_coder_index None) 77)::
  (EA 77 AWeaken 78)::(EA 78 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_index) s) < (eval (ENum (0))
  s))%Z)) 80)::(EA 78 (AGuard (fun s => ((eval (EVar V_adpcm_coder_index)
  s) >= (eval (ENum (0)) s))%Z)) 79)::(EA 79 AWeaken 84)::
  (EA 80 AWeaken 81)::(EA 81 (AAssign V_adpcm_coder_index
  (Some (ENum (0)))) 82)::(EA 82 ANone 83)::(EA 83 AWeaken 84)::
  (EA 84 (AGuard (fun s => ((eval (EVar V_adpcm_coder_index) s) >
  (eval (ENum (88)) s))%Z)) 86)::(EA 84 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_index) s) <= (eval (ENum (88))
  s))%Z)) 85)::(EA 85 AWeaken 89)::(EA 86 AWeaken 87)::(EA 87 (AAssign
  V_adpcm_coder_index (Some (ENum (88)))) 88)::(EA 88 ANone 89)::
  (EA 89 (AAssign V_adpcm_coder_step None) 90)::(EA 90 AWeaken 91)::
  (EA 91 (AGuard (fun s => ((eval (EVar V_adpcm_coder_bufferstep) s) <>
  (eval (ENum (0)) s))%Z)) 94)::(EA 91 (AGuard
  (fun s => ((eval (EVar V_adpcm_coder_bufferstep) s) = (eval (ENum (0))
  s))%Z)) 92)::(EA 92 AWeaken 93)::(EA 93 ANone 97)::(EA 94 AWeaken 95)::
  (EA 95 (AAssign V_adpcm_coder_outputbuffer None) 96)::(EA 96 ANone 97)::
  (EA 97 (AAssign V_adpcm_coder_bufferstep None) 98)::(EA 98 ANone 99)::
  (EA 99 (AAssign V_adpcm_coder__tmp (Some (EAdd (EVar V_adpcm_coder__tmp)
  (ENum (-1))))) 100)::(EA 100 ANone 101)::(EA 101 ANone 102)::
  (EA 102 (AAssign V_adpcm_coder_z (Some (EAdd (ENum (1))
  (EVar V_adpcm_coder_z)))) 103)::(EA 103 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_adpcm_coder => Pedges_adpcm_coder
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_adpcm_coder => 19
     end)%positive;
  var_global := var_global
}.

Definition ai_adpcm_coder (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 3 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_z <= 0)%Z
   | 4 => (1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_outputbuffer <= 0 /\ -1 * s V_adpcm_coder_outputbuffer <= 0)%Z
   | 5 => (-1 * s V_adpcm_coder_outputbuffer <= 0 /\ 1 * s V_adpcm_coder_outputbuffer <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_z <= 0)%Z
   | 6 => (1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_outputbuffer <= 0 /\ -1 * s V_adpcm_coder_outputbuffer <= 0)%Z
   | 7 => (-1 * s V_adpcm_coder_outputbuffer <= 0 /\ 1 * s V_adpcm_coder_outputbuffer <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_z <= 0)%Z
   | 8 => (1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_outputbuffer <= 0 /\ -1 * s V_adpcm_coder_outputbuffer <= 0 /\ 1 * s V_adpcm_coder_bufferstep + -1 <= 0 /\ -1 * s V_adpcm_coder_bufferstep + 1 <= 0)%Z
   | 9 => (-1 * s V_adpcm_coder_bufferstep + 1 <= 0 /\ 1 * s V_adpcm_coder_bufferstep + -1 <= 0 /\ -1 * s V_adpcm_coder_outputbuffer <= 0 /\ 1 * s V_adpcm_coder_outputbuffer <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_z <= 0)%Z
   | 10 => (-1 * s V_adpcm_coder_z <= 0)%Z
   | 11 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder__tmp <= 0)%Z
   | 12 => (1 * s V_adpcm_coder__tmp <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 13 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder__tmp <= 0 /\ 1 * s V_adpcm_coder_bufferstep <= 0 /\ -1 * s V_adpcm_coder_bufferstep <= 0)%Z
   | 14 => (-1 * s V_adpcm_coder_bufferstep <= 0 /\ 1 * s V_adpcm_coder_bufferstep <= 0 /\ 1 * s V_adpcm_coder__tmp <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 15 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder__tmp <= 0)%Z
   | 16 => (1 * s V_adpcm_coder__tmp <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 17 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder__tmp <= 0)%Z
   | 18 => (1 * s V_adpcm_coder__tmp <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 19 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder__tmp <= 0)%Z
   | 20 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 22 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 24 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 26 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ 1 * s V_adpcm_coder_sign <= 0 /\ -1 * s V_adpcm_coder_sign <= 0)%Z
   | 27 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 28 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 29 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 30 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 31 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ 1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder_delta <= 0)%Z
   | 32 => (-1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 33 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ 1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder_delta <= 0)%Z
   | 34 => (-1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_diff+ -1 * s V_adpcm_coder_step + 1 <= 0)%Z
   | 35 => (-1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0)%Z
   | 36 => (-1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ 1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder_delta <= 0)%Z
   | 37 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_delta + 4 <= 0)%Z
   | 38 => (-1 * s V_adpcm_coder_delta + 4 <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_diff <= 0)%Z
   | 39 => (-1 * s V_adpcm_coder_diff <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_delta + 4 <= 0)%Z
   | 40 => (-1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 41 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_delta <= 0)%Z
   | 42 => (-1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 43 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_diff+ -1 * s V_adpcm_coder_step + 1 <= 0)%Z
   | 44 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_delta <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0)%Z
   | 45 => (-1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0 /\ -1 * s V_adpcm_coder_delta <= 0 /\ 1 * s V_adpcm_coder_delta + -4 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 46 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0)%Z
   | 47 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_diff <= 0)%Z
   | 48 => (-1 * s V_adpcm_coder_diff <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 49 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 50 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 51 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 52 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_diff+ -1 * s V_adpcm_coder_step + 1 <= 0)%Z
   | 53 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0)%Z
   | 54 => (-1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 55 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0)%Z
   | 56 => (-1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 57 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_diff+ 1 * s V_adpcm_coder_step <= 0)%Z
   | 58 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 59 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_sign <= 0 /\ -1 * s V_adpcm_coder_sign <= 0)%Z
   | 60 => (-1 * s V_adpcm_coder_sign <= 0 /\ 1 * s V_adpcm_coder_sign <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 61 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_sign <= 0 /\ -1 * s V_adpcm_coder_sign <= 0)%Z
   | 62 => (-1 * s V_adpcm_coder_sign <= 0 /\ 1 * s V_adpcm_coder_sign <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 63 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 64 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 65 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 66 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 67 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 68 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 69 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 70 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 71 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 72 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 73 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 74 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 75 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 76 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 77 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 78 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 79 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 80 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ 1 * s V_adpcm_coder_index + 1 <= 0)%Z
   | 81 => (1 * s V_adpcm_coder_index + 1 <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 82 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ 1 * s V_adpcm_coder_index <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 83 => (-1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 84 => (-1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 85 => (-1 * s V_adpcm_coder_index <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0)%Z
   | 86 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder_index + 89 <= 0)%Z
   | 87 => (-1 * s V_adpcm_coder_index + 89 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 88 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index + 88 <= 0)%Z
   | 89 => (-1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 90 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 91 => (-1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 92 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_bufferstep <= 0 /\ -1 * s V_adpcm_coder_bufferstep <= 0)%Z
   | 93 => (-1 * s V_adpcm_coder_bufferstep <= 0 /\ 1 * s V_adpcm_coder_bufferstep <= 0 /\ -1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 94 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 95 => (-1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 96 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 97 => (-1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 98 => (-1 * s V_adpcm_coder__tmp + 1 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0)%Z
   | 99 => (-1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0 /\ -1 * s V_adpcm_coder__tmp + 1 <= 0)%Z
   | 100 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0 /\ -1 * s V_adpcm_coder__tmp <= 0)%Z
   | 101 => (-1 * s V_adpcm_coder__tmp <= 0 /\ -1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z <= 0)%Z
   | 102 => (-1 * s V_adpcm_coder_z <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_index <= 0 /\ -1 * s V_adpcm_coder__tmp <= 0)%Z
   | 103 => (-1 * s V_adpcm_coder__tmp <= 0 /\ -1 * s V_adpcm_coder_index <= 0 /\ 1 * s V_adpcm_coder_index + -88 <= 0 /\ -1 * s V_adpcm_coder_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_adpcm_coder (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_adpcm_coder_len) <= z)%Q
   | 2 => (s V_adpcm_coder_z + max0(s V_adpcm_coder_len) <= z)%Q
   | 3 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 4 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 5 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 6 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 7 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 8 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 9 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 10 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_adpcm_coder__tmp)]
     (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 12 => (s V_adpcm_coder_z - max0(-1 + s V_adpcm_coder__tmp)
            + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 13 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_adpcm_coder__tmp) (-1
                                                                   + 
                                                                   s V_adpcm_coder__tmp))]
     (s V_adpcm_coder_z - max0(-1 + s V_adpcm_coder__tmp)
      + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 14 => (s V_adpcm_coder_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_adpcm_coder__tmp) (-1
                                                                    + 
                                                                    s V_adpcm_coder__tmp))]
     (s V_adpcm_coder_z - max0(-1 + s V_adpcm_coder__tmp)
      + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 16 => (s V_adpcm_coder_z <= z)%Q
   | 17 => (s V_adpcm_coder_z <= z)%Q
   | 18 => (s V_adpcm_coder_z <= z)%Q
   | 19 => (s V_adpcm_coder_z <= z)%Q
   | 20 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_adpcm_coder__tmp)) (F_check_ge (s V_adpcm_coder__tmp) (0))]
     (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 21 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 22 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 23 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 24 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 25 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 26 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 27 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 28 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 29 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 30 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 31 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 32 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 33 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 34 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 35 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 36 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 37 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 38 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 39 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 40 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 41 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 42 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 43 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 44 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 45 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 46 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 47 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 48 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 49 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 50 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 51 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 52 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 53 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 54 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 55 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 56 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 57 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 58 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 59 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 60 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 61 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 62 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 63 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 64 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 65 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 66 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 67 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 68 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 69 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 70 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 71 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 72 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 73 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 74 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 75 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 76 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 77 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 78 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 79 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 80 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 81 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 82 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 83 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 84 => (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 85 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_adpcm_coder__tmp) (1)]
     (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 86 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_adpcm_coder__tmp) (1)]
     (s V_adpcm_coder__tmp + s V_adpcm_coder_z <= z)%Q
   | 87 => ((1 # 1) + s V_adpcm_coder__tmp + s V_adpcm_coder_z
            + max0(-1 + s V_adpcm_coder__tmp) - max0(s V_adpcm_coder__tmp) <= z)%Q
   | 88 => ((1 # 1) + s V_adpcm_coder__tmp + s V_adpcm_coder_z
            + max0(-1 + s V_adpcm_coder__tmp) - max0(s V_adpcm_coder__tmp) <= z)%Q
   | 89 => ((1 # 1) + s V_adpcm_coder__tmp + s V_adpcm_coder_z
            + max0(-1 + s V_adpcm_coder__tmp) - max0(s V_adpcm_coder__tmp) <= z)%Q
   | 90 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_adpcm_coder__tmp) (0))) (F_max0_ge_0 (s V_adpcm_coder__tmp))]
     ((1 # 1) + s V_adpcm_coder__tmp + s V_adpcm_coder_z
      + max0(-1 + s V_adpcm_coder__tmp) - max0(s V_adpcm_coder__tmp) <= z)%Q
   | 91 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 92 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 93 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 94 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 95 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 96 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 97 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 98 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 99 => ((1 # 1) + s V_adpcm_coder_z + max0(-1 + s V_adpcm_coder__tmp) <= z)%Q
   | 100 => ((1 # 1) + s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 101 => ((1 # 1) + s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 102 => ((1 # 1) + s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | 103 => (s V_adpcm_coder_z + max0(s V_adpcm_coder__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_adpcm_coder =>
    [mkPA Q (fun n z s => ai_adpcm_coder n s /\ annot0_adpcm_coder n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_adpcm_coder (proc_start P_adpcm_coder) s1 (proc_end P_adpcm_coder) s2 ->
    (s2 V_adpcm_coder_z <= max0(s1 V_adpcm_coder_len))%Q.
Proof.
  prove_bound ipa admissible_ipa P_adpcm_coder.
Qed.
