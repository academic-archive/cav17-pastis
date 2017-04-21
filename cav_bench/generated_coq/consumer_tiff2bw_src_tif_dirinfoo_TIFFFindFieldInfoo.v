Require Import pasta.Pasta.

Inductive proc: Type :=
  P__TIFFFindFieldInfo.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V__TIFFFindFieldInfo_z := 1%positive.
Notation V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0 := 2%positive.
Notation V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off8 := 3%positive.
Notation V__TIFFFindFieldInfo__tmp := 4%positive.
Notation V__TIFFFindFieldInfo__tmp1 := 5%positive.
Notation V__TIFFFindFieldInfo_i := 6%positive.
Notation V__TIFFFindFieldInfo_n := 7%positive.
Notation V__TIFFFindFieldInfo_tif_dref_off848 := 8%positive.
Notation V__TIFFFindFieldInfo_dt := 9%positive.
Notation V__TIFFFindFieldInfo_tag := 10%positive.
Notation V__TIFFFindFieldInfo_tif := 11%positive.
Definition Pedges__TIFFFindFieldInfo: list (edge proc) :=
  (EA 1 (AAssign V__TIFFFindFieldInfo_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V__TIFFFindFieldInfo__tmp (Some (EVar V__TIFFFindFieldInfo_tag))) 3)::
  (EA 3 (AAssign V__TIFFFindFieldInfo__tmp1
  (Some (EVar V__TIFFFindFieldInfo_dt))) 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 6)::(EA 5 ANone 14)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0)
  s) = (eval (EVar V__TIFFFindFieldInfo__tmp) s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0)
  s) <> (eval (EVar V__TIFFFindFieldInfo__tmp) s))%Z)) 8)::
  (EA 8 AWeaken 14)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__tmp1) s) = (eval (ENum (0))
  s))%Z)) 38)::(EA 10 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__tmp1) s) =
  (eval (EVar V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off8)
  s))%Z)) 37)::(EA 12 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__tmp1) s) <>
  (eval (EVar V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off8)
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 (AAssign V__TIFFFindFieldInfo_i
  (Some (ENum (0)))) 15)::(EA 15 (AAssign V__TIFFFindFieldInfo_n
  (Some (EVar V__TIFFFindFieldInfo_tif_dref_off848))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo_i) s) <
  (eval (EVar V__TIFFFindFieldInfo_n) s))%Z)) 22)::(EA 18 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo_i) s) >=
  (eval (EVar V__TIFFFindFieldInfo_n) s))%Z)) 19)::(EA 19 AWeaken 20)::
  (EA 20 ANone 21)::(EA 21 AWeaken 41)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 23 ANone 28)::(EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__tmp1) s) = (eval (ENum (0))
  s))%Z)) 34)::(EA 25 (AGuard
  (fun s => ((eval (EVar V__TIFFFindFieldInfo__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 ANone 35)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V__TIFFFindFieldInfo_i
  (Some (EAdd (EVar V__TIFFFindFieldInfo_i) (ENum (1))))) 30)::
  (EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign V__TIFFFindFieldInfo_z
  (Some (EAdd (ENum (1)) (EVar V__TIFFFindFieldInfo_z)))) 33)::
  (EA 33 AWeaken 18)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::
  (EA 36 AWeaken 41)::(EA 37 AWeaken 39)::(EA 38 AWeaken 39)::
  (EA 39 ANone 40)::(EA 40 AWeaken 41)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P__TIFFFindFieldInfo => Pedges__TIFFFindFieldInfo
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P__TIFFFindFieldInfo => 41
     end)%positive;
  var_global := var_global
}.

Definition ai__TIFFFindFieldInfo (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 3 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 4 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 5 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 6 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 7 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 8 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 9 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0)%Z
   | 10 => (-1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 11 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0)%Z
   | 12 => (-1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 13 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0)%Z
   | 14 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 15 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0)%Z
   | 16 => (-1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 17 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0)%Z
   | 18 => (-1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 19 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_i+ 1 * s V__TIFFFindFieldInfo_n <= 0)%Z
   | 20 => (-1 * s V__TIFFFindFieldInfo_i+ 1 * s V__TIFFFindFieldInfo_n <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 21 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_i+ 1 * s V__TIFFFindFieldInfo_n <= 0)%Z
   | 22 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0)%Z
   | 23 => (1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 24 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0)%Z
   | 25 => (1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 26 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0)%Z
   | 27 => (1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 28 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0)%Z
   | 29 => (1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 30 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n <= 0 /\ -1 * s V__TIFFFindFieldInfo_i + 1 <= 0)%Z
   | 31 => (-1 * s V__TIFFFindFieldInfo_i + 1 <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 32 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n <= 0 /\ -1 * s V__TIFFFindFieldInfo_i + 1 <= 0)%Z
   | 33 => (-1 * s V__TIFFFindFieldInfo_i + 1 <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n <= 0 /\ -1 * s V__TIFFFindFieldInfo_z + 1 <= 0)%Z
   | 34 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0 /\ 1 * s V__TIFFFindFieldInfo__tmp1 <= 0 /\ -1 * s V__TIFFFindFieldInfo__tmp1 <= 0)%Z
   | 35 => (1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 36 => (-1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_i <= 0 /\ 1 * s V__TIFFFindFieldInfo_i+ -1 * s V__TIFFFindFieldInfo_n + 1 <= 0)%Z
   | 37 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off8+ 1 * s V__TIFFFindFieldInfo__tmp1 <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off8+ -1 * s V__TIFFFindFieldInfo__tmp1 <= 0)%Z
   | 38 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ 1 * s V__TIFFFindFieldInfo__tmp1 <= 0 /\ -1 * s V__TIFFFindFieldInfo__tmp1 <= 0)%Z
   | 39 => (-1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | 40 => (1 * s V__TIFFFindFieldInfo_z <= 0 /\ -1 * s V__TIFFFindFieldInfo_z <= 0 /\ 1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ -1 * s V__TIFFFindFieldInfo__tmp <= 0 /\ -1 * s V__TIFFFindFieldInfo__TIFFFindFieldInfo_dot_last_dref_off0+ 1 * s V__TIFFFindFieldInfo__tmp <= 0)%Z
   | 41 => (-1 * s V__TIFFFindFieldInfo_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0__TIFFFindFieldInfo (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 2 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 3 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 4 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 5 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 6 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 7 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 8 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 9 => (s V__TIFFFindFieldInfo_z
           + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 10 => (s V__TIFFFindFieldInfo_z
            + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 11 => (s V__TIFFFindFieldInfo_z
            + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 12 => (s V__TIFFFindFieldInfo_z
            + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 13 => (s V__TIFFFindFieldInfo_z
            + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 14 => (s V__TIFFFindFieldInfo_z
            + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 15 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i
                   + s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 16 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 17 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 18 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 19 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 20 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V__TIFFFindFieldInfo_i
                                             + s V__TIFFFindFieldInfo_n) (-1
                                                                    - s V__TIFFFindFieldInfo_i
                                                                    + s V__TIFFFindFieldInfo_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V__TIFFFindFieldInfo_i
                            + s V__TIFFFindFieldInfo_n)]
     (s V__TIFFFindFieldInfo_z
      + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V__TIFFFindFieldInfo_i
                                       + s V__TIFFFindFieldInfo_n) (1)]
     (s V__TIFFFindFieldInfo_z
      + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 23 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 24 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 25 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 26 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 27 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 28 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 29 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 30 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 31 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 32 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 33 => (s V__TIFFFindFieldInfo_z
            + max0(-s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 34 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 35 => ((1 # 1) + s V__TIFFFindFieldInfo_z
            + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V__TIFFFindFieldInfo_i
                            + s V__TIFFFindFieldInfo_n)]
     ((1 # 1) + s V__TIFFFindFieldInfo_z
      + max0(-1 - s V__TIFFFindFieldInfo_i + s V__TIFFFindFieldInfo_n) <= z)%Q
   | 37 => hints
     [(*0 1*) F_max0_ge_0 (s V__TIFFFindFieldInfo_tif_dref_off848)]
     (s V__TIFFFindFieldInfo_z + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 38 => hints
     [(*0 1*) F_max0_ge_0 (s V__TIFFFindFieldInfo_tif_dref_off848)]
     (s V__TIFFFindFieldInfo_z + max0(s V__TIFFFindFieldInfo_tif_dref_off848) <= z)%Q
   | 39 => (s V__TIFFFindFieldInfo_z <= z)%Q
   | 40 => (s V__TIFFFindFieldInfo_z <= z)%Q
   | 41 => (s V__TIFFFindFieldInfo_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P__TIFFFindFieldInfo =>
    [mkPA Q (fun n z s => ai__TIFFFindFieldInfo n s /\ annot0__TIFFFindFieldInfo n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P__TIFFFindFieldInfo (proc_start P__TIFFFindFieldInfo) s1 (proc_end P__TIFFFindFieldInfo) s2 ->
    (s2 V__TIFFFindFieldInfo_z <= max0(s1 V__TIFFFindFieldInfo_tif_dref_off848))%Q.
Proof.
  prove_bound ipa admissible_ipa P__TIFFFindFieldInfo.
Qed.
