Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fill_buffer_resample.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fill_buffer_resample_z := 1%positive.
Notation V_fill_buffer_resample__tmp := 2%positive.
Notation V_fill_buffer_resample__tmp1 := 3%positive.
Notation V_fill_buffer_resample__tmp2 := 4%positive.
Notation V_fill_buffer_resample_gfp_dref_off168 := 5%positive.
Notation V_fill_buffer_resample_i := 6%positive.
Notation V_fill_buffer_resample_j := 7%positive.
Notation V_fill_buffer_resample_k := 8%positive.
Notation V_fill_buffer_resample_linear := 9%positive.
Notation V_fill_buffer_resample_num_used_dref := 10%positive.
Notation V_fill_buffer_resample_value := 11%positive.
Notation V_fill_buffer_resample_y0 := 12%positive.
Notation V_fill_buffer_resample_y1 := 13%positive.
Notation V_fill_buffer_resample_y2 := 14%positive.
Notation V_fill_buffer_resample_y3 := 15%positive.
Notation V_fill_buffer_resample_ch := 16%positive.
Notation V_fill_buffer_resample_desired_len := 17%positive.
Notation V_fill_buffer_resample_gfp := 18%positive.
Notation V_fill_buffer_resample_inbuf := 19%positive.
Notation V_fill_buffer_resample_len := 20%positive.
Notation V_fill_buffer_resample_num_used := 21%positive.
Notation V_fill_buffer_resample_outbuf := 22%positive.
Definition Pedges_fill_buffer_resample: list (edge proc) :=
  (EA 1 (AAssign V_fill_buffer_resample_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_fill_buffer_resample__tmp
  (Some (EVar V_fill_buffer_resample_desired_len))) 3)::(EA 3 (AAssign
  V_fill_buffer_resample__tmp1 (Some (EVar V_fill_buffer_resample_len))) 4)::
  (EA 4 (AAssign V_fill_buffer_resample__tmp2
  (Some (EVar V_fill_buffer_resample_ch))) 5)::(EA 5 (AAssign
  V_fill_buffer_resample_j (Some (ENum (0)))) 6)::(EA 6 AWeaken 7)::
  (EA 7 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_gfp_dref_off168) s) =
  (eval (ENum (0)) s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_gfp_dref_off168) s) <>
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 14)::(EA 9 AWeaken 10)::
  (EA 10 ANone 13)::(EA 10 ANone 11)::(EA 11 ANone 12)::(EA 12 AWeaken 14)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_gfp_dref_off168) s) <>
  (eval (ENum (0)) s))%Z)) 16)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_gfp_dref_off168) s) =
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 18)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_fill_buffer_resample_linear None) 19)::
  (EA 19 (AAssign V_fill_buffer_resample_k (Some (ENum (0)))) 20)::
  (EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_k) s) <
  (eval (EVar V_fill_buffer_resample__tmp) s))%Z)) 24)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_k) s) >=
  (eval (EVar V_fill_buffer_resample__tmp) s))%Z)) 23)::(EA 23 AWeaken 79)::
  (EA 24 AWeaken 25)::(EA 25 (AAssign V_fill_buffer_resample_j None) 26)::
  (EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EAdd (EVar V_fill_buffer_resample_j) (ENum (2))) s) >=
  (eval (EVar V_fill_buffer_resample__tmp1) s))%Z)) 76)::(EA 27 (AGuard
  (fun s => ((eval (EAdd (EVar V_fill_buffer_resample_j) (ENum (2))) s) <
  (eval (EVar V_fill_buffer_resample__tmp1) s))%Z)) 28)::(EA 28 AWeaken 29)::
  (EA 29 (AGuard (fun s => ((eval (EVar V_fill_buffer_resample_j) s) <
  (eval (ENum (0)) s))%Z)) 32)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_j) s) >= (eval (ENum (0))
  s))%Z)) 30)::(EA 30 AWeaken 31)::(EA 31 ANone 34)::(EA 32 AWeaken 33)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_fill_buffer_resample_y1 None) 35)::
  (EA 35 AWeaken 36)::(EA 36 (AGuard (fun s => ((eval (EAdd (ENum (1))
  (EVar V_fill_buffer_resample_j)) s) < (eval (ENum (0)) s))%Z)) 39)::
  (EA 36 (AGuard (fun s => ((eval (EAdd (ENum (1))
  (EVar V_fill_buffer_resample_j)) s) >= (eval (ENum (0)) s))%Z)) 37)::
  (EA 37 AWeaken 38)::(EA 38 ANone 41)::(EA 39 AWeaken 40)::
  (EA 40 ANone 41)::(EA 41 (AAssign V_fill_buffer_resample_y2 None) 42)::
  (EA 42 AWeaken 43)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_linear) s) <>
  (eval (ENum (0)) s))%Z)) 68)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_linear) s) = (eval (ENum (0))
  s))%Z)) 44)::(EA 44 AWeaken 45)::(EA 45 (AGuard
  (fun s => ((eval (ESub (EVar V_fill_buffer_resample_j) (ENum (1))) s) <
  (eval (ENum (0)) s))%Z)) 48)::(EA 45 (AGuard
  (fun s => ((eval (ESub (EVar V_fill_buffer_resample_j) (ENum (1))) s) >=
  (eval (ENum (0)) s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 ANone 50)::
  (EA 48 AWeaken 49)::(EA 49 ANone 50)::(EA 50 (AAssign
  V_fill_buffer_resample_y0 None) 51)::(EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EAdd (EVar V_fill_buffer_resample_j) (ENum (2))) s) <
  (eval (ENum (0)) s))%Z)) 55)::(EA 52 (AGuard
  (fun s => ((eval (EAdd (EVar V_fill_buffer_resample_j) (ENum (2))) s) >=
  (eval (ENum (0)) s))%Z)) 53)::(EA 53 AWeaken 54)::(EA 54 ANone 57)::
  (EA 55 AWeaken 56)::(EA 56 ANone 57)::(EA 57 (AAssign
  V_fill_buffer_resample_y3 None) 58)::(EA 58 (AAssign
  V_fill_buffer_resample_value None) 59)::(EA 59 AWeaken 60)::
  (EA 60 ANone 66)::(EA 60 ANone 61)::(EA 61 AWeaken 62)::(EA 62 ANone 64)::
  (EA 62 ANone 63)::(EA 63 ANone 65)::(EA 64 ANone 65)::(EA 65 ANone 67)::
  (EA 66 ANone 67)::(EA 67 ANone 70)::(EA 68 AWeaken 69)::(EA 69 ANone 70)::
  (EA 70 ANone 71)::(EA 71 (AAssign V_fill_buffer_resample_k
  (Some (EAdd (EVar V_fill_buffer_resample_k) (ENum (1))))) 72)::
  (EA 72 ANone 73)::(EA 73 ANone 74)::(EA 74 (AAssign
  V_fill_buffer_resample_z (Some (EAdd (ENum (1))
  (EVar V_fill_buffer_resample_z)))) 75)::(EA 75 AWeaken 22)::
  (EA 76 AWeaken 77)::(EA 77 ANone 78)::(EA 78 AWeaken 79)::(EA 79 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample__tmp1) s) <
  (eval (EAdd (EVar V_fill_buffer_resample_j) (ENum (2))) s))%Z)) 82)::
  (EA 79 (AGuard (fun s => ((eval (EVar V_fill_buffer_resample__tmp1) s) >=
  (eval (EAdd (EVar V_fill_buffer_resample_j) (ENum (2))) s))%Z)) 80)::
  (EA 80 AWeaken 81)::(EA 81 ANone 84)::(EA 82 AWeaken 83)::
  (EA 83 ANone 84)::(EA 84 (AAssign V_fill_buffer_resample_num_used_dref
  None) 85)::(EA 85 (AAssign V_fill_buffer_resample_i
  (Some (ENum (0)))) 86)::(EA 86 ANone 87)::(EA 87 AWeaken 88)::
  (EA 88 (AGuard (fun s => ((eval (EVar V_fill_buffer_resample_i) s) <
  (eval (ENum (5)) s))%Z)) 91)::(EA 88 (AGuard
  (fun s => ((eval (EVar V_fill_buffer_resample_i) s) >= (eval (ENum (5))
  s))%Z)) 89)::(EA 89 AWeaken 90)::(EA 91 AWeaken 92)::(EA 92 ANone 93)::
  (EA 93 (AAssign V_fill_buffer_resample_i
  (Some (EAdd (EVar V_fill_buffer_resample_i) (ENum (1))))) 94)::
  (EA 94 ANone 95)::(EA 95 ANone 96)::(EA 96 (AAssign
  V_fill_buffer_resample_z (Some (EAdd (ENum (1))
  (EVar V_fill_buffer_resample_z)))) 97)::(EA 97 AWeaken 88)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fill_buffer_resample => Pedges_fill_buffer_resample
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fill_buffer_resample => 90
     end)%positive;
  var_global := var_global
}.

Definition ai_fill_buffer_resample (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0)%Z
   | 3 => (-1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 4 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0)%Z
   | 5 => (-1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 6 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0)%Z
   | 7 => (-1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 8 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0)%Z
   | 9 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ -1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0)%Z
   | 10 => (-1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ 1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 11 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ -1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0)%Z
   | 12 => (-1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ 1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 13 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ -1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0)%Z
   | 14 => (-1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 15 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0 /\ -1 * s V_fill_buffer_resample_gfp_dref_off168 <= 0)%Z
   | 16 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0)%Z
   | 17 => (-1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 18 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0)%Z
   | 19 => (-1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 20 => (1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 21 => (-1 * s V_fill_buffer_resample_k <= 0 /\ 1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_z <= 0)%Z
   | 22 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 23 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample__tmp+ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 24 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0)%Z
   | 25 => (-1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 26 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0)%Z
   | 27 => (-1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 28 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 29 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 30 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample_j <= 0)%Z
   | 31 => (-1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 32 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_j + 1 <= 0)%Z
   | 33 => (1 * s V_fill_buffer_resample_j + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 34 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 35 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 36 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 37 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_j + -1 <= 0)%Z
   | 38 => (-1 * s V_fill_buffer_resample_j + -1 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 39 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ 1 * s V_fill_buffer_resample_j + 2 <= 0)%Z
   | 40 => (1 * s V_fill_buffer_resample_j + 2 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 41 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 42 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 43 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 44 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 45 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 46 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_j + 1 <= 0)%Z
   | 47 => (-1 * s V_fill_buffer_resample_j + 1 <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 48 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_j <= 0)%Z
   | 49 => (1 * s V_fill_buffer_resample_j <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 50 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 51 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 52 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 53 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_j + -2 <= 0)%Z
   | 54 => (-1 * s V_fill_buffer_resample_j + -2 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 55 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 56 => (1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 57 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 58 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 59 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 60 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 61 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 62 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 63 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 64 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 65 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 66 => (-1 * s V_fill_buffer_resample_linear <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 67 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ 1 * s V_fill_buffer_resample_linear <= 0 /\ -1 * s V_fill_buffer_resample_linear <= 0)%Z
   | 68 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 69 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 70 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0)%Z
   | 71 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 72 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_k + 1 <= 0)%Z
   | 73 => (-1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0)%Z
   | 74 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_k + 1 <= 0)%Z
   | 75 => (-1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 3 <= 0 /\ -1 * s V_fill_buffer_resample_z + 1 <= 0)%Z
   | 76 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ 1 * s V_fill_buffer_resample__tmp1+ -1 * s V_fill_buffer_resample_j + -2 <= 0)%Z
   | 77 => (1 * s V_fill_buffer_resample__tmp1+ -1 * s V_fill_buffer_resample_j + -2 <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 78 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp+ 1 * s V_fill_buffer_resample_k + 1 <= 0 /\ 1 * s V_fill_buffer_resample__tmp1+ -1 * s V_fill_buffer_resample_j + -2 <= 0)%Z
   | 79 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 80 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 2 <= 0)%Z
   | 81 => (-1 * s V_fill_buffer_resample__tmp1+ 1 * s V_fill_buffer_resample_j + 2 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 82 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample__tmp1+ -1 * s V_fill_buffer_resample_j + -1 <= 0)%Z
   | 83 => (1 * s V_fill_buffer_resample__tmp1+ -1 * s V_fill_buffer_resample_j + -1 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 84 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0)%Z
   | 85 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 86 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_i <= 0 /\ -1 * s V_fill_buffer_resample_i <= 0)%Z
   | 87 => (-1 * s V_fill_buffer_resample_i <= 0 /\ 1 * s V_fill_buffer_resample_i <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 88 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_i <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ 1 * s V_fill_buffer_resample_i + -5 <= 0)%Z
   | 89 => (1 * s V_fill_buffer_resample_i + -5 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_i + 5 <= 0)%Z
   | 90 => (-1 * s V_fill_buffer_resample_i + 5 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ 1 * s V_fill_buffer_resample_i + -5 <= 0)%Z
   | 91 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_i <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_i + -4 <= 0)%Z
   | 92 => (1 * s V_fill_buffer_resample_i + -4 <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_i <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0)%Z
   | 93 => (-1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_i <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0 /\ 1 * s V_fill_buffer_resample_i + -4 <= 0)%Z
   | 94 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_i + 1 <= 0 /\ 1 * s V_fill_buffer_resample_i + -5 <= 0)%Z
   | 95 => (1 * s V_fill_buffer_resample_i + -5 <= 0 /\ -1 * s V_fill_buffer_resample_i + 1 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z <= 0)%Z
   | 96 => (-1 * s V_fill_buffer_resample_z <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_i + 1 <= 0 /\ 1 * s V_fill_buffer_resample_i + -5 <= 0)%Z
   | 97 => (1 * s V_fill_buffer_resample_i + -5 <= 0 /\ -1 * s V_fill_buffer_resample_i + 1 <= 0 /\ -1 * s V_fill_buffer_resample_k <= 0 /\ -1 * s V_fill_buffer_resample_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fill_buffer_resample (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((5 # 1) + max0(s V_fill_buffer_resample_desired_len) <= z)%Q
   | 2 => ((5 # 1) + s V_fill_buffer_resample_z
           + max0(s V_fill_buffer_resample_desired_len) <= z)%Q
   | 3 => ((5 # 1) + s V_fill_buffer_resample_z
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 4 => ((5 # 1) + s V_fill_buffer_resample_z
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 5 => ((5 # 1) + s V_fill_buffer_resample_z
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 6 => (-(5 # 1) * s V_fill_buffer_resample_j + s V_fill_buffer_resample_z
           + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 7 => (-(5 # 1) * s V_fill_buffer_resample_j + s V_fill_buffer_resample_z
           + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 8 => (-(5 # 1) * s V_fill_buffer_resample_j + s V_fill_buffer_resample_z
           + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 9 => (-(5 # 1) * s V_fill_buffer_resample_j + s V_fill_buffer_resample_z
           + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
           + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 10 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 11 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 12 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 13 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 14 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 15 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 16 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 17 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 18 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 19 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp) <= z)%Q
   | 20 => (-(5 # 1) * s V_fill_buffer_resample_j
            + s V_fill_buffer_resample_z
            + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 21 => hints
     [(*-5 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_fill_buffer_resample_j)) (F_check_ge (1
                                                                    + s V_fill_buffer_resample_j) (0))]
     (-(5 # 1) * s V_fill_buffer_resample_j + s V_fill_buffer_resample_z
      + (5 # 1) * max0(1 + s V_fill_buffer_resample_j)
      + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 22 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fill_buffer_resample__tmp
                                             - s V_fill_buffer_resample_k) (-1
                                                                    + s V_fill_buffer_resample__tmp
                                                                    - s V_fill_buffer_resample_k));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fill_buffer_resample__tmp
                            - s V_fill_buffer_resample_k)]
     ((5 # 1) + s V_fill_buffer_resample_z
      + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 24 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 25 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 26 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 27 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 28 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_fill_buffer_resample__tmp
                                      - s V_fill_buffer_resample_k) (1)]
     ((5 # 1) + s V_fill_buffer_resample_z
      + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 29 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 30 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 31 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 32 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 33 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 34 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 35 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 36 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 37 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 38 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 39 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 40 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 41 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 42 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 43 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 44 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 45 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 46 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 47 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 48 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 49 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 50 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 51 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 52 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 53 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 54 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 55 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 56 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 57 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 58 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 59 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 60 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 61 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 62 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 63 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 64 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 65 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 66 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 67 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 68 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 69 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 70 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 71 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(-1 + s V_fill_buffer_resample__tmp
                   - s V_fill_buffer_resample_k) <= z)%Q
   | 72 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 73 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 74 => ((6 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 75 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 76 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 77 => ((5 # 1) + s V_fill_buffer_resample_z
            + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 78 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_fill_buffer_resample__tmp
                                       - s V_fill_buffer_resample_k) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_fill_buffer_resample__tmp
                            - s V_fill_buffer_resample_k)]
     ((5 # 1) + s V_fill_buffer_resample_z
      + max0(s V_fill_buffer_resample__tmp - s V_fill_buffer_resample_k) <= z)%Q
   | 79 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 80 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 81 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 82 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 83 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 84 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 85 => ((5 # 1) + s V_fill_buffer_resample_z <= z)%Q
   | 86 => (s V_fill_buffer_resample_z + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 87 => (s V_fill_buffer_resample_z + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 88 => (s V_fill_buffer_resample_z + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 89 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (5 - s V_fill_buffer_resample_i) (4
                                                                    - s V_fill_buffer_resample_i));
      (*-1 0*) F_max0_ge_0 (4 - s V_fill_buffer_resample_i)]
     (s V_fill_buffer_resample_z + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 90 => (s V_fill_buffer_resample_z <= z)%Q
   | 91 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (5 - s V_fill_buffer_resample_i) (1)]
     (s V_fill_buffer_resample_z + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 92 => ((1 # 1) + s V_fill_buffer_resample_z
            + max0(4 - s V_fill_buffer_resample_i) <= z)%Q
   | 93 => ((1 # 1) + s V_fill_buffer_resample_z
            + max0(4 - s V_fill_buffer_resample_i) <= z)%Q
   | 94 => ((1 # 1) + s V_fill_buffer_resample_z
            + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 95 => ((1 # 1) + s V_fill_buffer_resample_z
            + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 96 => ((1 # 1) + s V_fill_buffer_resample_z
            + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | 97 => (s V_fill_buffer_resample_z + max0(5 - s V_fill_buffer_resample_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fill_buffer_resample =>
    [mkPA Q (fun n z s => ai_fill_buffer_resample n s /\ annot0_fill_buffer_resample n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fill_buffer_resample (proc_start P_fill_buffer_resample) s1 (proc_end P_fill_buffer_resample) s2 ->
    (s2 V_fill_buffer_resample_z <= (5 # 1)
                                    + max0(s1 V_fill_buffer_resample_desired_len))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fill_buffer_resample.
Qed.
