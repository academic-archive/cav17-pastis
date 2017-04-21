Require Import pasta.Pasta.

Inductive proc: Type :=
  P_adpcm_decoder.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_adpcm_decoder_z := 1%positive.
Notation V_adpcm_decoder__tmp := 2%positive.
Notation V_adpcm_decoder_bufferstep := 3%positive.
Notation V_adpcm_decoder_delta := 4%positive.
Notation V_adpcm_decoder_index := 5%positive.
Notation V_adpcm_decoder_inputbuffer := 6%positive.
Notation V_adpcm_decoder_sign := 7%positive.
Notation V_adpcm_decoder_state_dref_off0 := 8%positive.
Notation V_adpcm_decoder_state_dref_off2 := 9%positive.
Notation V_adpcm_decoder_step := 10%positive.
Notation V_adpcm_decoder_valpred := 11%positive.
Notation V_adpcm_decoder_vpdiff := 12%positive.
Notation V_adpcm_decoder_indata := 13%positive.
Notation V_adpcm_decoder_len := 14%positive.
Notation V_adpcm_decoder_outdata := 15%positive.
Notation V_adpcm_decoder_state := 16%positive.
Definition Pedges_adpcm_decoder: list (edge proc) :=
  (EA 1 (AAssign V_adpcm_decoder_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_adpcm_decoder__tmp (Some (EVar V_adpcm_decoder_len))) 3)::(EA 3 (AAssign
  V_adpcm_decoder_inputbuffer (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_adpcm_decoder_valpred (Some (EVar V_adpcm_decoder_state_dref_off0))) 5)::
  (EA 5 (AAssign V_adpcm_decoder_index
  (Some (EVar V_adpcm_decoder_state_dref_off2))) 6)::(EA 6 (AAssign
  V_adpcm_decoder_step None) 7)::(EA 7 (AAssign V_adpcm_decoder_bufferstep
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_adpcm_decoder__tmp) s) > (eval (ENum (0))
  s))%Z)) 16)::(EA 10 (AGuard (fun s => ((eval (EVar V_adpcm_decoder__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_adpcm_decoder_state_dref_off0
  (Some (EVar V_adpcm_decoder_valpred))) 13)::(EA 13 (AAssign
  V_adpcm_decoder_state_dref_off2 (Some (EVar V_adpcm_decoder_index))) 14)::
  (EA 14 AWeaken 15)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_adpcm_decoder_bufferstep) s) <> (eval (ENum (0))
  s))%Z)) 22)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_adpcm_decoder_bufferstep) s) = (eval (ENum (0))
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_adpcm_decoder_inputbuffer None) 20)::(EA 20 (AAssign
  V_adpcm_decoder_delta None) 21)::(EA 21 ANone 25)::(EA 22 AWeaken 23)::
  (EA 23 (AAssign V_adpcm_decoder_delta None) 24)::(EA 24 ANone 25)::
  (EA 25 (AAssign V_adpcm_decoder_bufferstep None) 26)::(EA 26 (AAssign
  V_adpcm_decoder_index None) 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_adpcm_decoder_index) s) < (eval (ENum (0))
  s))%Z)) 30)::(EA 28 (AGuard (fun s => ((eval (EVar V_adpcm_decoder_index)
  s) >= (eval (ENum (0)) s))%Z)) 29)::(EA 29 AWeaken 34)::
  (EA 30 AWeaken 31)::(EA 31 (AAssign V_adpcm_decoder_index
  (Some (ENum (0)))) 32)::(EA 32 ANone 33)::(EA 33 AWeaken 34)::
  (EA 34 (AGuard (fun s => ((eval (EVar V_adpcm_decoder_index) s) >
  (eval (ENum (88)) s))%Z)) 36)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_adpcm_decoder_index) s) <= (eval (ENum (88))
  s))%Z)) 35)::(EA 35 AWeaken 39)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_adpcm_decoder_index (Some (ENum (88)))) 38)::(EA 38 ANone 39)::
  (EA 39 (AAssign V_adpcm_decoder_sign None) 40)::(EA 40 (AAssign
  V_adpcm_decoder_delta None) 41)::(EA 41 (AAssign V_adpcm_decoder_vpdiff
  None) 42)::(EA 42 AWeaken 43)::(EA 43 ANone 45)::(EA 43 ANone 44)::
  (EA 44 AWeaken 48)::(EA 45 (AAssign V_adpcm_decoder_vpdiff
  (Some (EAdd (EVar V_adpcm_decoder_vpdiff)
  (EVar V_adpcm_decoder_step)))) 46)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::
  (EA 48 ANone 50)::(EA 48 ANone 49)::(EA 49 AWeaken 53)::(EA 50 (AAssign
  V_adpcm_decoder_vpdiff None) 51)::(EA 51 ANone 52)::(EA 52 AWeaken 53)::
  (EA 53 ANone 55)::(EA 53 ANone 54)::(EA 54 AWeaken 58)::(EA 55 (AAssign
  V_adpcm_decoder_vpdiff None) 56)::(EA 56 ANone 57)::(EA 57 AWeaken 58)::
  (EA 58 (AGuard (fun s => ((eval (EVar V_adpcm_decoder_sign) s) <>
  (eval (ENum (0)) s))%Z)) 63)::(EA 58 (AGuard
  (fun s => ((eval (EVar V_adpcm_decoder_sign) s) = (eval (ENum (0))
  s))%Z)) 59)::(EA 59 AWeaken 60)::(EA 60 (AAssign V_adpcm_decoder_valpred
  (Some (EAdd (EVar V_adpcm_decoder_valpred)
  (EVar V_adpcm_decoder_vpdiff)))) 61)::(EA 61 ANone 62)::
  (EA 62 AWeaken 67)::(EA 63 AWeaken 64)::(EA 64 (AAssign
  V_adpcm_decoder_valpred (Some (ESub (EVar V_adpcm_decoder_valpred)
  (EVar V_adpcm_decoder_vpdiff)))) 65)::(EA 65 ANone 66)::
  (EA 66 AWeaken 67)::(EA 67 ANone 73)::(EA 67 ANone 68)::
  (EA 68 AWeaken 69)::(EA 69 ANone 70)::(EA 69 ANone 72)::(EA 70 (AAssign
  V_adpcm_decoder_valpred None) 71)::(EA 71 ANone 72)::(EA 72 ANone 75)::
  (EA 73 (AAssign V_adpcm_decoder_valpred None) 74)::(EA 74 ANone 75)::
  (EA 75 (AAssign V_adpcm_decoder_step None) 76)::(EA 76 ANone 77)::
  (EA 77 (AAssign V_adpcm_decoder__tmp
  (Some (EAdd (EVar V_adpcm_decoder__tmp) (ENum (-1))))) 78)::
  (EA 78 ANone 79)::(EA 79 ANone 80)::(EA 80 (AAssign V_adpcm_decoder_z
  (Some (EAdd (ENum (1)) (EVar V_adpcm_decoder_z)))) 81)::
  (EA 81 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_adpcm_decoder => Pedges_adpcm_decoder
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_adpcm_decoder => 15
     end)%positive;
  var_global := var_global
}.

Definition ai_adpcm_decoder (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 3 => (-1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_z <= 0)%Z
   | 4 => (1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_inputbuffer <= 0 /\ -1 * s V_adpcm_decoder_inputbuffer <= 0)%Z
   | 5 => (-1 * s V_adpcm_decoder_inputbuffer <= 0 /\ 1 * s V_adpcm_decoder_inputbuffer <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_z <= 0)%Z
   | 6 => (1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_inputbuffer <= 0 /\ -1 * s V_adpcm_decoder_inputbuffer <= 0)%Z
   | 7 => (-1 * s V_adpcm_decoder_inputbuffer <= 0 /\ 1 * s V_adpcm_decoder_inputbuffer <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_z <= 0)%Z
   | 8 => (1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_inputbuffer <= 0 /\ -1 * s V_adpcm_decoder_inputbuffer <= 0 /\ 1 * s V_adpcm_decoder_bufferstep <= 0 /\ -1 * s V_adpcm_decoder_bufferstep <= 0)%Z
   | 9 => (-1 * s V_adpcm_decoder_bufferstep <= 0 /\ 1 * s V_adpcm_decoder_bufferstep <= 0 /\ -1 * s V_adpcm_decoder_inputbuffer <= 0 /\ 1 * s V_adpcm_decoder_inputbuffer <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_z <= 0)%Z
   | 10 => (-1 * s V_adpcm_decoder_z <= 0)%Z
   | 11 => (-1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder__tmp <= 0)%Z
   | 12 => (1 * s V_adpcm_decoder__tmp <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 13 => (-1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder__tmp <= 0)%Z
   | 14 => (1 * s V_adpcm_decoder__tmp <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 15 => (-1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder__tmp <= 0)%Z
   | 16 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 18 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_bufferstep <= 0 /\ -1 * s V_adpcm_decoder_bufferstep <= 0)%Z
   | 19 => (-1 * s V_adpcm_decoder_bufferstep <= 0 /\ 1 * s V_adpcm_decoder_bufferstep <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 20 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_bufferstep <= 0 /\ -1 * s V_adpcm_decoder_bufferstep <= 0)%Z
   | 21 => (-1 * s V_adpcm_decoder_bufferstep <= 0 /\ 1 * s V_adpcm_decoder_bufferstep <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 22 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 24 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 26 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 27 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 28 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 29 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 30 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_index + 1 <= 0)%Z
   | 31 => (1 * s V_adpcm_decoder_index + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 32 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_index <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 33 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0)%Z
   | 34 => (-1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 35 => (-1 * s V_adpcm_decoder_index <= 0 /\ -1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0)%Z
   | 36 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_index + 89 <= 0)%Z
   | 37 => (-1 * s V_adpcm_decoder_index + 89 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 38 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index + 88 <= 0)%Z
   | 39 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 40 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 41 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 42 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 43 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 44 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 45 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 46 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 47 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 48 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 49 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 50 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 51 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 52 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 53 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 54 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 55 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 56 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 57 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 58 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 59 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_sign <= 0 /\ -1 * s V_adpcm_decoder_sign <= 0)%Z
   | 60 => (-1 * s V_adpcm_decoder_sign <= 0 /\ 1 * s V_adpcm_decoder_sign <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 61 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_sign <= 0 /\ -1 * s V_adpcm_decoder_sign <= 0)%Z
   | 62 => (-1 * s V_adpcm_decoder_sign <= 0 /\ 1 * s V_adpcm_decoder_sign <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 63 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 64 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 65 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 66 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 67 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 68 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 69 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 70 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 71 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 72 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 73 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 74 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 75 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 76 => (-1 * s V_adpcm_decoder_z <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0)%Z
   | 77 => (-1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder__tmp + 1 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 78 => (-1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ -1 * s V_adpcm_decoder__tmp <= 0)%Z
   | 79 => (-1 * s V_adpcm_decoder__tmp <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_z <= 0)%Z
   | 80 => (-1 * s V_adpcm_decoder_z <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ -1 * s V_adpcm_decoder__tmp <= 0)%Z
   | 81 => (-1 * s V_adpcm_decoder__tmp <= 0 /\ -1 * s V_adpcm_decoder_index <= 0 /\ 1 * s V_adpcm_decoder_index + -88 <= 0 /\ -1 * s V_adpcm_decoder_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_adpcm_decoder (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_adpcm_decoder_len) <= z)%Q
   | 2 => (max0(s V_adpcm_decoder_len) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 3 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 4 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 5 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 6 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 7 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 8 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 9 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 10 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 11 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 12 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 13 => (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_adpcm_decoder__tmp) (-1
                                                                    + s V_adpcm_decoder__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_adpcm_decoder__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_adpcm_decoder_z)) (F_check_ge (s V_adpcm_decoder_z) (0))]
     (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 15 => (s V_adpcm_decoder_z <= z)%Q
   | 16 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_adpcm_decoder__tmp) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_adpcm_decoder__tmp)) (F_check_ge (-1
                                                                    + s V_adpcm_decoder__tmp) (0))]
     (max0(s V_adpcm_decoder__tmp) + max0(s V_adpcm_decoder_z) <= z)%Q
   | 17 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 18 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 19 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 20 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 21 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 22 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 23 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 24 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 25 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 26 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 27 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 28 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 29 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 30 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 31 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 32 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 33 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 34 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 35 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_adpcm_decoder_z)) (F_check_ge (s V_adpcm_decoder_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_adpcm_decoder_z) (0))) (F_max0_ge_0 (s V_adpcm_decoder_z))]
     (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 36 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 37 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 38 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 39 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 40 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 41 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 42 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 43 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 44 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 45 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 46 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 47 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 48 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 49 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 50 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 51 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 52 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 53 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 54 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_adpcm_decoder_z)) (F_check_ge (s V_adpcm_decoder_z) (0))]
     (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 55 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 56 => (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 57 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_adpcm_decoder_z)) (F_check_ge (s V_adpcm_decoder_z) (0))]
     (s V_adpcm_decoder__tmp + max0(s V_adpcm_decoder_z) <= z)%Q
   | 58 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 59 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 60 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 61 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 62 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 63 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 64 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 65 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 66 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 67 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 68 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 69 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 70 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 71 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 72 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 73 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 74 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 75 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 76 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 77 => (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 78 => ((1 # 1) + s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 79 => ((1 # 1) + s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 80 => ((1 # 1) + s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | 81 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_adpcm_decoder_z) (0))) (F_max0_ge_0 (s V_adpcm_decoder_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_adpcm_decoder__tmp) (0))) (F_max0_ge_0 (s V_adpcm_decoder__tmp))]
     (s V_adpcm_decoder__tmp + s V_adpcm_decoder_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_adpcm_decoder =>
    [mkPA Q (fun n z s => ai_adpcm_decoder n s /\ annot0_adpcm_decoder n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_adpcm_decoder (proc_start P_adpcm_decoder) s1 (proc_end P_adpcm_decoder) s2 ->
    (s2 V_adpcm_decoder_z <= max0(s1 V_adpcm_decoder_len))%Q.
Proof.
  prove_bound ipa admissible_ipa P_adpcm_decoder.
Qed.
