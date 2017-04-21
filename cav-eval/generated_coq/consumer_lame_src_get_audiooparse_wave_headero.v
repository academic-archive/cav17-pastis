Require Import pasta.Pasta.

Inductive proc: Type :=
  P_parse_wave_header.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_parse_wave_header_z := 1%positive.
Notation V_parse_wave_header__tmp := 2%positive.
Notation V_parse_wave_header_data_length := 3%positive.
Notation V_parse_wave_header_file_length := 4%positive.
Notation V_parse_wave_header_is_wav := 5%positive.
Notation V_parse_wave_header_loop_sanity := 6%positive.
Notation V_parse_wave_header_num_channels := 7%positive.
Notation V_parse_wave_header_num_samples := 8%positive.
Notation V_parse_wave_header_samp_freq := 9%positive.
Notation V_parse_wave_header_subSize := 10%positive.
Notation V_parse_wave_header_type := 11%positive.
Notation V_parse_wave_header_sf := 12%positive.
Definition Pedges_parse_wave_header: list (edge proc) :=
  (EA 1 (AAssign V_parse_wave_header_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_parse_wave_header_is_wav (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_parse_wave_header_data_length (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_parse_wave_header_subSize (Some (ENum (0)))) 5)::(EA 5 (AAssign
  V_parse_wave_header_loop_sanity (Some (ENum (0)))) 6)::(EA 6 (AAssign
  V_parse_wave_header_file_length None) 7)::(EA 7 AWeaken 8)::
  (EA 8 ANone 73)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_parse_wave_header_loop_sanity (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_loop_sanity) s) <
  (eval (ENum (20)) s))%Z)) 14)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_loop_sanity) s) >=
  (eval (ENum (20)) s))%Z)) 13)::(EA 13 AWeaken 33)::(EA 14 AWeaken 15)::
  (EA 15 (AAssign V_parse_wave_header_type None) 16)::(EA 16 AWeaken 17)::
  (EA 17 ANone 43)::(EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 19 ANone 28)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_parse_wave_header_subSize None) 21)::
  (EA 21 AWeaken 22)::(EA 22 ANone 25)::(EA 22 ANone 23)::(EA 23 ANone 24)::
  (EA 24 ANone 60)::(EA 25 (AAssign V_parse_wave_header__tmp
  (Some (ENum (0)))) 26)::(EA 26 ANone 27)::(EA 27 AWeaken 76)::
  (EA 28 (AAssign V_parse_wave_header_subSize None) 29)::(EA 29 (AAssign
  V_parse_wave_header_data_length
  (Some (EVar V_parse_wave_header_subSize))) 30)::(EA 30 (AAssign
  V_parse_wave_header_is_wav (Some (ENum (1)))) 31)::(EA 31 ANone 32)::
  (EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_is_wav) s) <> (eval (ENum (0))
  s))%Z)) 35)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_is_wav) s) = (eval (ENum (0))
  s))%Z)) 34)::(EA 34 AWeaken 40)::(EA 35 AWeaken 36)::(EA 36 (AAssign
  V_parse_wave_header_num_channels None) 37)::(EA 37 (AAssign
  V_parse_wave_header_samp_freq None) 38)::(EA 38 (AAssign
  V_parse_wave_header_num_samples None) 39)::(EA 39 ANone 40)::
  (EA 40 (AAssign V_parse_wave_header__tmp
  (Some (EVar V_parse_wave_header_is_wav))) 41)::(EA 41 ANone 42)::
  (EA 42 AWeaken 76)::(EA 43 (AAssign V_parse_wave_header_subSize None) 44)::
  (EA 44 AWeaken 45)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_subSize) s) < (eval (ENum (16))
  s))%Z)) 69)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_subSize) s) >= (eval (ENum (16))
  s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 (AAssign
  V_parse_wave_header_subSize (Some (ESub (EVar V_parse_wave_header_subSize)
  (ENum (2))))) 48)::(EA 48 (AAssign V_parse_wave_header_subSize
  (Some (ESub (EVar V_parse_wave_header_subSize) (ENum (2))))) 49)::
  (EA 49 (AAssign V_parse_wave_header_subSize
  (Some (ESub (EVar V_parse_wave_header_subSize) (ENum (4))))) 50)::
  (EA 50 (AAssign V_parse_wave_header_subSize
  (Some (ESub (EVar V_parse_wave_header_subSize) (ENum (4))))) 51)::
  (EA 51 (AAssign V_parse_wave_header_subSize
  (Some (ESub (EVar V_parse_wave_header_subSize) (ENum (2))))) 52)::
  (EA 52 (AAssign V_parse_wave_header_subSize
  (Some (ESub (EVar V_parse_wave_header_subSize) (ENum (2))))) 53)::
  (EA 53 AWeaken 54)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_subSize) s) > (eval (ENum (0))
  s))%Z)) 56)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_parse_wave_header_subSize) s) <= (eval (ENum (0))
  s))%Z)) 55)::(EA 55 AWeaken 59)::(EA 56 AWeaken 57)::(EA 57 ANone 66)::
  (EA 57 ANone 58)::(EA 58 ANone 59)::(EA 59 ANone 60)::(EA 60 ANone 61)::
  (EA 61 (AAssign V_parse_wave_header_loop_sanity
  (Some (EAdd (EVar V_parse_wave_header_loop_sanity) (ENum (1))))) 62)::
  (EA 62 ANone 63)::(EA 63 ANone 64)::(EA 64 (AAssign V_parse_wave_header_z
  (Some (EAdd (ENum (1)) (EVar V_parse_wave_header_z)))) 65)::
  (EA 65 AWeaken 12)::(EA 66 (AAssign V_parse_wave_header__tmp
  (Some (ENum (0)))) 67)::(EA 67 ANone 68)::(EA 68 AWeaken 76)::
  (EA 69 AWeaken 70)::(EA 70 (AAssign V_parse_wave_header__tmp
  (Some (ENum (0)))) 71)::(EA 71 ANone 72)::(EA 72 AWeaken 76)::
  (EA 73 (AAssign V_parse_wave_header__tmp (Some (ENum (0)))) 74)::
  (EA 74 ANone 75)::(EA 75 AWeaken 76)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_parse_wave_header => Pedges_parse_wave_header
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_parse_wave_header => 76
     end)%positive;
  var_global := var_global
}.

Definition ai_parse_wave_header (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_z <= 0)%Z
   | 3 => (-1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 4 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0)%Z
   | 5 => (-1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 6 => (-1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 7 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 8 => (-1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 9 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 10 => (-1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 11 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 12 => (-1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0)%Z
   | 13 => (1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity + 20 <= 0)%Z
   | 14 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 15 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 16 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 17 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 18 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 19 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 20 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 21 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 22 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 23 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 24 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 25 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 26 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ -1 * s V_parse_wave_header__tmp <= 0)%Z
   | 27 => (-1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 28 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 29 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 30 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 31 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ -1 * s V_parse_wave_header_is_wav + 1 <= 0)%Z
   | 32 => (-1 * s V_parse_wave_header_is_wav + 1 <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 33 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0)%Z
   | 34 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 35 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_is_wav + 1 <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0)%Z
   | 36 => (1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ -1 * s V_parse_wave_header_is_wav + 1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 37 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_is_wav + 1 <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0)%Z
   | 38 => (1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ -1 * s V_parse_wave_header_is_wav + 1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 39 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_is_wav + 1 <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0)%Z
   | 40 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 41 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header__tmp + -1 <= 0 /\ -1 * s V_parse_wave_header__tmp <= 0)%Z
   | 42 => (-1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header__tmp + -1 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0)%Z
   | 43 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 44 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 45 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 46 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_subSize + 16 <= 0)%Z
   | 47 => (-1 * s V_parse_wave_header_subSize + 16 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 48 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_subSize + 14 <= 0)%Z
   | 49 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize + 12 <= 0)%Z
   | 50 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_subSize + 8 <= 0)%Z
   | 51 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize + 4 <= 0)%Z
   | 52 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_subSize + 2 <= 0)%Z
   | 53 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 54 => (-1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 55 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0)%Z
   | 56 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize + 1 <= 0)%Z
   | 57 => (-1 * s V_parse_wave_header_subSize + 1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 58 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize + 1 <= 0)%Z
   | 59 => (-1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 60 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 61 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 62 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_loop_sanity + 1 <= 0)%Z
   | 63 => (-1 * s V_parse_wave_header_loop_sanity + 1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0)%Z
   | 64 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_loop_sanity + 1 <= 0)%Z
   | 65 => (-1 * s V_parse_wave_header_loop_sanity + 1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_z + 1 <= 0)%Z
   | 66 => (-1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize + 1 <= 0)%Z
   | 67 => (-1 * s V_parse_wave_header_subSize + 1 <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ -1 * s V_parse_wave_header__tmp <= 0)%Z
   | 68 => (-1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_subSize + 1 <= 0)%Z
   | 69 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize + -15 <= 0)%Z
   | 70 => (1 * s V_parse_wave_header_subSize + -15 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 71 => (1 * s V_parse_wave_header_loop_sanity + -19 <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize + -15 <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ -1 * s V_parse_wave_header__tmp <= 0)%Z
   | 72 => (-1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header_subSize + -15 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_loop_sanity + -19 <= 0)%Z
   | 73 => (-1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 74 => (-1 * s V_parse_wave_header_subSize <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ -1 * s V_parse_wave_header__tmp <= 0)%Z
   | 75 => (-1 * s V_parse_wave_header__tmp <= 0 /\ 1 * s V_parse_wave_header__tmp <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ 1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header_data_length <= 0 /\ 1 * s V_parse_wave_header_data_length <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_z <= 0 /\ 1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ 1 * s V_parse_wave_header_subSize <= 0 /\ -1 * s V_parse_wave_header_subSize <= 0)%Z
   | 76 => (1 * s V_parse_wave_header_loop_sanity + -20 <= 0 /\ 1 * s V_parse_wave_header_is_wav + -1 <= 0 /\ 1 * s V_parse_wave_header__tmp + -1 <= 0 /\ -1 * s V_parse_wave_header_is_wav <= 0 /\ -1 * s V_parse_wave_header_z <= 0 /\ -1 * s V_parse_wave_header_loop_sanity <= 0 /\ -1 * s V_parse_wave_header__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_parse_wave_header (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((20 # 1) <= z)%Q
   | 2 => ((20 # 1) + s V_parse_wave_header_z <= z)%Q
   | 3 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 4 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 5 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 6 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 7 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 8 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 9 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
           + s V_parse_wave_header_z
           + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 10 => (-(560 # 19) + (20 # 1) * s V_parse_wave_header_is_wav
            - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav)
            + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 11 => hints
     [(*0 20*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_parse_wave_header_is_wav)) (F_check_ge (0) (0));
      (*-20 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_parse_wave_header_is_wav) (0))) (F_max0_ge_0 (-
                                                                    s V_parse_wave_header_is_wav));
      (*-40 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                    - s V_parse_wave_header_is_wav)) (F_check_ge (1
                                                                    - s V_parse_wave_header_is_wav) (0))]
     (-(560 # 19) + (20 # 1) * s V_parse_wave_header_is_wav
      - (10 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav)
      + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 12 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (20
                                             - s V_parse_wave_header_loop_sanity) (19
                                                                    - s V_parse_wave_header_loop_sanity));
      (*-1 0*) F_max0_ge_0 (19 - s V_parse_wave_header_loop_sanity)]
     ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 14 => hints
     [(*0 0.473684*) F_binom_monotonic 1 (F_max0_ge_arg (19
                                                         - s V_parse_wave_header_loop_sanity)) (F_check_ge (19
                                                                    - s V_parse_wave_header_loop_sanity) (0))]
     ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 15 => ((371 # 19) - s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 16 => hints
     [(*-0.526316 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - s V_parse_wave_header_loop_sanity) (0))) (F_max0_ge_0 (20
                                                                    - s V_parse_wave_header_loop_sanity))]
     ((371 # 19) - s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + (9 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 17 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (20 - s V_parse_wave_header_loop_sanity) (1);
      (*0 0.0526316*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_wave_header_loop_sanity) (0))) (F_max0_ge_0 (s V_parse_wave_header_loop_sanity))]
     ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 19 => ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 20 => ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 21 => hints
     [(*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_wave_header_loop_sanity)) (F_check_ge (s V_parse_wave_header_loop_sanity) (0))]
     ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 22 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 23 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 24 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 25 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 26 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_one;
      (*-0.473684 0*) F_max0_pre_decrement 1 (20
                                              - s V_parse_wave_header_loop_sanity) (1);
      (*-1 0*) F_max0_ge_0 (19 - s V_parse_wave_header_loop_sanity);
      (*-0.473684 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - s V_parse_wave_header_loop_sanity) (0))) (F_max0_ge_0 (20
                                                                    - s V_parse_wave_header_loop_sanity))]
     ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 28 => ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 29 => ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 30 => ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 31 => ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 32 => hints
     [(*-0.526316 0*) F_max0_pre_decrement 1 (20
                                              - s V_parse_wave_header_loop_sanity) (1);
      (*-1.05263 0*) F_max0_ge_0 (19 - s V_parse_wave_header_loop_sanity);
      (*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_parse_wave_header_loop_sanity)) (F_check_ge (0) (0))]
     ((10 # 1) - (10 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + (1 # 19) * max0(s V_parse_wave_header_loop_sanity) <= z)%Q
   | 33 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 34 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 35 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 36 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 37 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 38 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 39 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 40 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 41 => ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 42 => hints
     [(*-0.526316 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - s V_parse_wave_header_loop_sanity) (0))) (F_max0_ge_0 (20
                                                                    - s V_parse_wave_header_loop_sanity))]
     ((200 # 19) - (10 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      - (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 43 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 44 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 45 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 46 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 47 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 48 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 49 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 50 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 51 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 52 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 53 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 54 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 55 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (20 - s V_parse_wave_header_loop_sanity) (1)]
     ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (20 - s V_parse_wave_header_loop_sanity) (1)]
     ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 57 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 58 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 59 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 60 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 61 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 62 => ((199 # 19) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 63 => ((199 # 19) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 64 => ((199 # 19) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 65 => hints
     [(*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_arg (20
                                                           - s V_parse_wave_header_loop_sanity)) (F_check_ge (20
                                                                    - s V_parse_wave_header_loop_sanity) (0))]
     ((180 # 19) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (10 # 19) * max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 66 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 67 => ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_one;
      (*-0.473684 0*) F_max0_pre_decrement 1 (20
                                              - s V_parse_wave_header_loop_sanity) (1);
      (*-1 0*) F_max0_ge_0 (19 - s V_parse_wave_header_loop_sanity);
      (*-0.473684 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - s V_parse_wave_header_loop_sanity) (0))) (F_max0_ge_0 (20
                                                                    - s V_parse_wave_header_loop_sanity))]
     ((10 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      + (10 # 19) * max0(19 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 69 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 70 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 71 => ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
            + s V_parse_wave_header_z
            - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
            + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 72 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (20
                                             - s V_parse_wave_header_loop_sanity) (19
                                                                    - s V_parse_wave_header_loop_sanity));
      (*-1 0*) F_max0_ge_0 (19 - s V_parse_wave_header_loop_sanity);
      (*-0.473684 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (19
                                                                    - s V_parse_wave_header_loop_sanity) (0))) (F_max0_ge_0 (19
                                                                    - s V_parse_wave_header_loop_sanity))]
     ((9 # 1) - (9 # 19) * s V_parse_wave_header_loop_sanity
      + s V_parse_wave_header_z
      - (9 # 19) * max0(19 - s V_parse_wave_header_loop_sanity)
      + max0(20 - s V_parse_wave_header_loop_sanity) <= z)%Q
   | 73 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
            + s V_parse_wave_header_z
            + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 74 => (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
            + s V_parse_wave_header_z
            + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 75 => hints
     [(*-20 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                  - s V_parse_wave_header_is_wav)) (F_check_ge (0) (0));
      (*-20 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                    - s V_parse_wave_header_is_wav)) (F_check_ge (1
                                                                    - s V_parse_wave_header_is_wav) (0))]
     (-(20 # 1) + (20 # 1) * s V_parse_wave_header_is_wav
      + s V_parse_wave_header_z
      + (40 # 1) * max0(1 - s V_parse_wave_header_is_wav) <= z)%Q
   | 76 => (s V_parse_wave_header_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_parse_wave_header =>
    [mkPA Q (fun n z s => ai_parse_wave_header n s /\ annot0_parse_wave_header n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_parse_wave_header (proc_start P_parse_wave_header) s1 (proc_end P_parse_wave_header) s2 ->
    (s2 V_parse_wave_header_z <= (20 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_parse_wave_header.
Qed.
