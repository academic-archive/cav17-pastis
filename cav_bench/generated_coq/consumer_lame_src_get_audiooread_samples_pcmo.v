Require Import pasta.Pasta.

Inductive proc: Type :=
  P_read_samples_pcm.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_read_samples_pcm_z := 1%positive.
Notation V_read_samples_pcm_NativeByteOrder := 2%positive.
Notation V_read_samples_pcm__tmp := 3%positive.
Notation V_read_samples_pcm__tmp1 := 4%positive.
Notation V_read_samples_pcm_gfp_dref_off124 := 5%positive.
Notation V_read_samples_pcm_iswav := 6%positive.
Notation V_read_samples_pcm_rcode := 7%positive.
Notation V_read_samples_pcm_samples_read := 8%positive.
Notation V_read_samples_pcm_frame_size := 9%positive.
Notation V_read_samples_pcm_gfp := 10%positive.
Notation V_read_samples_pcm_sample_buffer := 11%positive.
Notation V_read_samples_pcm_samples_to_read := 12%positive.
Definition Pedges_read_samples_pcm: list (edge proc) :=
  (EA 1 (AAssign V_read_samples_pcm_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_read_samples_pcm__tmp (Some (EVar V_read_samples_pcm_frame_size))) 3)::
  (EA 3 (AAssign V_read_samples_pcm__tmp1
  (Some (EVar V_read_samples_pcm_samples_to_read))) 4)::(EA 4 (AAssign
  V_read_samples_pcm_iswav None) 5)::(EA 5 (AAssign
  V_read_samples_pcm_samples_read None) 6)::(EA 6 AWeaken 7)::
  (EA 7 ANone 65)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) =
  (eval (ENum (0)) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) <>
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 18)::(EA 11 AWeaken 12)::
  (EA 12 (AAssign V_read_samples_pcm_NativeByteOrder None) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) =
  (eval (ENum (0)) s))%Z)) 62)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) <>
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_iswav) s) <> (eval (ENum (0))
  s))%Z)) 25)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_iswav) s) = (eval (ENum (0))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) =
  (eval (ENum (2)) s))%Z)) 22)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) <>
  (eval (ENum (2)) s))%Z)) 21)::(EA 21 AWeaken 26)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 26)::(EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_iswav) s) <> (eval (ENum (0))
  s))%Z)) 28)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_iswav) s) = (eval (ENum (0))
  s))%Z)) 27)::(EA 27 AWeaken 34)::(EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) =
  (eval (ENum (1)) s))%Z)) 31)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_NativeByteOrder) s) <>
  (eval (ENum (1)) s))%Z)) 30)::(EA 30 AWeaken 34)::(EA 31 AWeaken 32)::
  (EA 32 ANone 33)::(EA 33 AWeaken 34)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_gfp_dref_off124) s) =
  (eval (ENum (1)) s))%Z)) 36)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_gfp_dref_off124) s) <>
  (eval (ENum (1)) s))%Z)) 35)::(EA 35 AWeaken 38)::(EA 36 AWeaken 37)::
  (EA 37 ANone 38)::(EA 38 (AAssign V_read_samples_pcm_rcode
  (Some (EVar V_read_samples_pcm_samples_read))) 39)::(EA 39 AWeaken 40)::
  (EA 40 (AGuard (fun s => ((eval (EVar V_read_samples_pcm_samples_read) s) <
  (eval (EVar V_read_samples_pcm__tmp) s))%Z)) 42)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_samples_read) s) >=
  (eval (EVar V_read_samples_pcm__tmp) s))%Z)) 41)::(EA 41 AWeaken 53)::
  (EA 42 AWeaken 43)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_samples_read) s) <
  (eval (ENum (0)) s))%Z)) 45)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_samples_read) s) >=
  (eval (ENum (0)) s))%Z)) 44)::(EA 44 AWeaken 48)::(EA 45 AWeaken 46)::
  (EA 46 (AAssign V_read_samples_pcm_samples_read (Some (ENum (0)))) 47)::
  (EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_samples_read) s) <
  (eval (EVar V_read_samples_pcm__tmp) s))%Z)) 55)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_read_samples_pcm_samples_read) s) >=
  (eval (EVar V_read_samples_pcm__tmp) s))%Z)) 51)::(EA 51 AWeaken 52)::
  (EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 AWeaken 66)::
  (EA 55 AWeaken 56)::(EA 56 ANone 57)::(EA 57 (AAssign
  V_read_samples_pcm_samples_read
  (Some (EAdd (EVar V_read_samples_pcm_samples_read) (ENum (1))))) 58)::
  (EA 58 ANone 59)::(EA 59 ANone 60)::(EA 60 (AAssign V_read_samples_pcm_z
  (Some (EAdd (ENum (1)) (EVar V_read_samples_pcm_z)))) 61)::
  (EA 61 AWeaken 50)::(EA 62 AWeaken 63)::(EA 63 ANone 64)::
  (EA 64 AWeaken 66)::(EA 65 AWeaken 66)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_read_samples_pcm => Pedges_read_samples_pcm
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_read_samples_pcm => 66
     end)%positive;
  var_global := var_global
}.

Definition ai_read_samples_pcm (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 3 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 4 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 5 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 6 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 7 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 8 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 9 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 10 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 11 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder <= 0)%Z
   | 12 => (-1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 13 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 14 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 15 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 16 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 17 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 18 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 19 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_iswav <= 0)%Z
   | 20 => (-1 * s V_read_samples_pcm_iswav <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 21 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_iswav <= 0)%Z
   | 22 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_iswav <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder + -2 <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder + 2 <= 0)%Z
   | 23 => (-1 * s V_read_samples_pcm_NativeByteOrder + 2 <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder + -2 <= 0 /\ -1 * s V_read_samples_pcm_iswav <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 24 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_iswav <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder + -2 <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder + 2 <= 0)%Z
   | 25 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 26 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 27 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_iswav <= 0 /\ -1 * s V_read_samples_pcm_iswav <= 0)%Z
   | 28 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 29 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 30 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 31 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder + -1 <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder + 1 <= 0)%Z
   | 32 => (-1 * s V_read_samples_pcm_NativeByteOrder + 1 <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder + -1 <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 33 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder + -1 <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder + 1 <= 0)%Z
   | 34 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 35 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 36 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_gfp_dref_off124 + -1 <= 0 /\ -1 * s V_read_samples_pcm_gfp_dref_off124 + 1 <= 0)%Z
   | 37 => (-1 * s V_read_samples_pcm_gfp_dref_off124 + 1 <= 0 /\ 1 * s V_read_samples_pcm_gfp_dref_off124 + -1 <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 38 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 39 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 40 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 41 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm__tmp+ -1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 42 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0)%Z
   | 43 => (-1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 44 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 45 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ 1 * s V_read_samples_pcm_samples_read + 1 <= 0)%Z
   | 46 => (1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 47 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 48 => (-1 * s V_read_samples_pcm_samples_read <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 49 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 50 => (-1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 51 => (-1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0 /\ 1 * s V_read_samples_pcm__tmp+ -1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 52 => (1 * s V_read_samples_pcm__tmp+ -1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 53 => (-1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm__tmp+ -1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 54 => (1 * s V_read_samples_pcm__tmp+ -1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 55 => (-1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0)%Z
   | 56 => (-1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 57 => (-1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read + 1 <= 0)%Z
   | 58 => (-1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 59 => (-1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 60 => (-1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read <= 0)%Z
   | 61 => (-1 * s V_read_samples_pcm__tmp+ 1 * s V_read_samples_pcm_samples_read <= 0 /\ -1 * s V_read_samples_pcm_samples_read + 1 <= 0 /\ -1 * s V_read_samples_pcm_z + 1 <= 0)%Z
   | 62 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder <= 0)%Z
   | 63 => (-1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_z <= 0)%Z
   | 64 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0 /\ 1 * s V_read_samples_pcm_NativeByteOrder <= 0 /\ -1 * s V_read_samples_pcm_NativeByteOrder <= 0)%Z
   | 65 => (1 * s V_read_samples_pcm_z <= 0 /\ -1 * s V_read_samples_pcm_z <= 0)%Z
   | 66 => (-1 * s V_read_samples_pcm_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_read_samples_pcm (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_read_samples_pcm_frame_size) <= z)%Q
   | 2 => (max0(s V_read_samples_pcm_frame_size)
           + max0(s V_read_samples_pcm_z) <= z)%Q
   | 3 => (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 4 => (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 5 => (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 6 => (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 7 => (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 8 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_read_samples_pcm_z)) (F_check_ge (0) (0))]
     (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 9 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 10 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 11 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 12 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 13 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 14 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 15 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 16 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 17 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 18 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 19 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 20 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 21 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 22 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 23 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 24 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 25 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 26 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_read_samples_pcm_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_read_samples_pcm_z) (0))) (F_max0_ge_0 (-
                                                                    s V_read_samples_pcm_z))]
     (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 28 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 29 => (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_read_samples_pcm_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_read_samples_pcm_z) (0))) (F_max0_ge_0 (-
                                                                    s V_read_samples_pcm_z))]
     (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 31 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_read_samples_pcm_z) (0))) (F_max0_ge_0 (-
                                                                    s V_read_samples_pcm_z))]
     (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 32 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp)
            + max0(-s V_read_samples_pcm_z) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_read_samples_pcm_z)) (F_check_ge (0) (0))]
     (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp)
      + max0(-s V_read_samples_pcm_z) <= z)%Q
   | 34 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 35 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 36 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 37 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 38 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 39 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 40 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_ge_0 (s V_read_samples_pcm__tmp)]
     (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 42 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 43 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_read_samples_pcm__tmp
                                                               - s V_read_samples_pcm_samples_read) (0))) (F_max0_ge_0 (s V_read_samples_pcm__tmp
                                                                    - s V_read_samples_pcm_samples_read));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_samples_pcm__tmp)) (F_check_ge (s V_read_samples_pcm__tmp) (0))]
     (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 45 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 46 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 47 => (s V_read_samples_pcm_samples_read + s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 48 => (s V_read_samples_pcm_samples_read + s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_read_samples_pcm_samples_read)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_read_samples_pcm_samples_read) (0))) (F_max0_ge_0 (s V_read_samples_pcm_samples_read))]
     (s V_read_samples_pcm_samples_read + s V_read_samples_pcm_z
      + max0(s V_read_samples_pcm__tmp - s V_read_samples_pcm_samples_read) <= z)%Q
   | 50 => (s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_read_samples_pcm__tmp
                                             - s V_read_samples_pcm_samples_read) (-1
                                                                    + s V_read_samples_pcm__tmp
                                                                    - s V_read_samples_pcm_samples_read));
      (*-1 0*) F_max0_ge_0 (-1 + s V_read_samples_pcm__tmp
                            - s V_read_samples_pcm_samples_read)]
     (s V_read_samples_pcm_z
      + max0(s V_read_samples_pcm__tmp - s V_read_samples_pcm_samples_read) <= z)%Q
   | 52 => (s V_read_samples_pcm_z <= z)%Q
   | 53 => (s V_read_samples_pcm_z <= z)%Q
   | 54 => (s V_read_samples_pcm_z <= z)%Q
   | 55 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_read_samples_pcm__tmp
                                       - s V_read_samples_pcm_samples_read) (1)]
     (s V_read_samples_pcm_z
      + max0(s V_read_samples_pcm__tmp - s V_read_samples_pcm_samples_read) <= z)%Q
   | 56 => ((1 # 1) + s V_read_samples_pcm_z
            + max0(-1 + s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 57 => ((1 # 1) + s V_read_samples_pcm_z
            + max0(-1 + s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 58 => ((1 # 1) + s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 59 => ((1 # 1) + s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 60 => ((1 # 1) + s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 61 => (s V_read_samples_pcm_z
            + max0(s V_read_samples_pcm__tmp
                   - s V_read_samples_pcm_samples_read) <= z)%Q
   | 62 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_read_samples_pcm_z) (0))) (F_max0_ge_0 (-
                                                                    s V_read_samples_pcm_z))]
     (max0(s V_read_samples_pcm__tmp) <= z)%Q
   | 63 => (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp)
            + max0(-s V_read_samples_pcm_z) <= z)%Q
   | 64 => hints
     [(*-1 0*) F_max0_ge_0 (s V_read_samples_pcm__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_read_samples_pcm_z)) (F_check_ge (0) (0))]
     (s V_read_samples_pcm_z + max0(s V_read_samples_pcm__tmp)
      + max0(-s V_read_samples_pcm_z) <= z)%Q
   | 65 => hints
     [(*-1 0*) F_max0_ge_0 (s V_read_samples_pcm__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_samples_pcm_z)) (F_check_ge (s V_read_samples_pcm_z) (0))]
     (max0(s V_read_samples_pcm__tmp) + max0(s V_read_samples_pcm_z) <= z)%Q
   | 66 => (s V_read_samples_pcm_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_read_samples_pcm =>
    [mkPA Q (fun n z s => ai_read_samples_pcm n s /\ annot0_read_samples_pcm n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_read_samples_pcm (proc_start P_read_samples_pcm) s1 (proc_end P_read_samples_pcm) s2 ->
    (s2 V_read_samples_pcm_z <= max0(s1 V_read_samples_pcm_frame_size))%Q.
Proof.
  prove_bound ipa admissible_ipa P_read_samples_pcm.
Qed.
