Require Import pasta.Pasta.

Inductive proc: Type :=
  P_id3_tag_detachframe.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_id3_tag_detachframe_z := 1%positive.
Notation V_id3_tag_detachframe__tmp := 2%positive.
Notation V_id3_tag_detachframe_i := 3%positive.
Notation V_id3_tag_detachframe_tag_dref_off24 := 4%positive.
Notation V_id3_tag_detachframe_frame := 5%positive.
Notation V_id3_tag_detachframe_tag := 6%positive.
Definition Pedges_id3_tag_detachframe: list (edge proc) :=
  (EA 1 (AAssign V_id3_tag_detachframe_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_id3_tag_detachframe_tag_dref_off24) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_id3_tag_detachframe_i
  (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) <
  (eval (EVar V_id3_tag_detachframe_tag_dref_off24) s))%Z)) 10)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) >=
  (eval (EVar V_id3_tag_detachframe_tag_dref_off24) s))%Z)) 9)::
  (EA 9 AWeaken 20)::(EA 10 AWeaken 11)::(EA 11 ANone 18)::(EA 11 ANone 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_id3_tag_detachframe_i
  (Some (EAdd (EVar V_id3_tag_detachframe_i) (ENum (1))))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_id3_tag_detachframe_z
  (Some (EAdd (ENum (1)) (EVar V_id3_tag_detachframe_z)))) 17)::
  (EA 17 AWeaken 8)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) =
  (eval (EVar V_id3_tag_detachframe_tag_dref_off24) s))%Z)) 35)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) <>
  (eval (EVar V_id3_tag_detachframe_tag_dref_off24) s))%Z)) 21)::
  (EA 21 AWeaken 22)::(EA 22 (AAssign V_id3_tag_detachframe_tag_dref_off24
  (Some (EAdd (EVar V_id3_tag_detachframe_tag_dref_off24)
  (ENum (-1))))) 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_id3_tag_detachframe_i (Some (EAdd (EVar V_id3_tag_detachframe_i)
  (ENum (1))))) 25)::(EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) <
  (eval (EVar V_id3_tag_detachframe_tag_dref_off24) s))%Z)) 31)::
  (EA 26 (AGuard (fun s => ((eval (EVar V_id3_tag_detachframe_i) s) >=
  (eval (EVar V_id3_tag_detachframe_tag_dref_off24) s))%Z)) 27)::
  (EA 27 AWeaken 28)::(EA 28 (AAssign V_id3_tag_detachframe__tmp
  (Some (ENum (0)))) 29)::(EA 29 ANone 30)::(EA 30 AWeaken 39)::
  (EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_id3_tag_detachframe_z (Some (EAdd (ENum (1))
  (EVar V_id3_tag_detachframe_z)))) 24)::(EA 35 AWeaken 36)::(EA 36 (AAssign
  V_id3_tag_detachframe__tmp (Some (ENum (-1)))) 37)::(EA 37 ANone 38)::
  (EA 38 AWeaken 39)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_id3_tag_detachframe => Pedges_id3_tag_detachframe
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_id3_tag_detachframe => 39
     end)%positive;
  var_global := var_global
}.

Definition ai_id3_tag_detachframe (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 3 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 4 => (-1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0)%Z
   | 5 => (-1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 6 => (-1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0)%Z
   | 7 => (-1 * s V_id3_tag_detachframe_i <= 0 /\ 1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 8 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 9 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 10 => (-1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0)%Z
   | 11 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0)%Z
   | 12 => (-1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0)%Z
   | 13 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0)%Z
   | 14 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0)%Z
   | 15 => (-1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 16 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0)%Z
   | 17 => (-1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_z + 1 <= 0)%Z
   | 18 => (-1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0)%Z
   | 19 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0)%Z
   | 20 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 21 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0)%Z
   | 22 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 23 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 24 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 25 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0)%Z
   | 26 => (-1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 27 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 28 => (-1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 29 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_detachframe__tmp <= 0 /\ -1 * s V_id3_tag_detachframe__tmp <= 0)%Z
   | 30 => (-1 * s V_id3_tag_detachframe__tmp <= 0 /\ 1 * s V_id3_tag_detachframe__tmp <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 31 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0)%Z
   | 32 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 33 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0)%Z
   | 34 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_i + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 35 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0)%Z
   | 36 => (-1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 37 => (-1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_detachframe__tmp + 1 <= 0 /\ -1 * s V_id3_tag_detachframe__tmp + -1 <= 0)%Z
   | 38 => (-1 * s V_id3_tag_detachframe__tmp + -1 <= 0 /\ 1 * s V_id3_tag_detachframe__tmp + 1 <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0)%Z
   | 39 => (1 * s V_id3_tag_detachframe_i+ -1 * s V_id3_tag_detachframe_tag_dref_off24 + -1 <= 0 /\ 1 * s V_id3_tag_detachframe__tmp <= 0 /\ -1 * s V_id3_tag_detachframe_z <= 0 /\ -1 * s V_id3_tag_detachframe_i <= 0 /\ -1 * s V_id3_tag_detachframe_i+ 1 * s V_id3_tag_detachframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_detachframe__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_id3_tag_detachframe (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 2 => (s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 3 => (s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 4 => (s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 5 => (s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 6 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 7 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 8 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 9 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
           + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 10 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 11 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 12 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 13 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 14 => ((1 # 1) - s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 15 => ((1 # 1) - s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 16 => ((1 # 1) - s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 17 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 18 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 19 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 20 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 21 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                  + s V_id3_tag_detachframe_tag_dref_off24)) (F_check_ge (1
                                                                    + s V_id3_tag_detachframe_tag_dref_off24) (0))]
     (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
      + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 22 => ((1 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 23 => ((2 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 24 => ((2 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 25 => ((3 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 26 => ((3 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 27 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_detachframe_i
                                                               - s V_id3_tag_detachframe_tag_dref_off24) (0))) (F_max0_ge_0 (s V_id3_tag_detachframe_i
                                                                    - s V_id3_tag_detachframe_tag_dref_off24))]
     ((3 # 1) - s V_id3_tag_detachframe_i
      + s V_id3_tag_detachframe_tag_dref_off24 + s V_id3_tag_detachframe_z <= z)%Q
   | 28 => ((3 # 1) - (3 # 1) * s V_id3_tag_detachframe_i
            + (3 # 1) * s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z
            + (2 # 1) * max0(s V_id3_tag_detachframe_i
                             - s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 29 => ((3 # 1) - (3 # 1) * s V_id3_tag_detachframe_i
            + (3 # 1) * s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z
            + (2 # 1) * max0(s V_id3_tag_detachframe_i
                             - s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 30 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_id3_tag_detachframe_i
                                                 - s V_id3_tag_detachframe_tag_dref_off24)) (F_check_ge (0) (0));
      (*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_id3_tag_detachframe_i
                                                 + s V_id3_tag_detachframe_tag_dref_off24)) (F_check_ge (0) (0));
      (*-3 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_id3_tag_detachframe_i
                                                               + s V_id3_tag_detachframe_tag_dref_off24) (0))) (F_max0_ge_0 (1
                                                                    - s V_id3_tag_detachframe_i
                                                                    + s V_id3_tag_detachframe_tag_dref_off24))]
     ((3 # 1) - (3 # 1) * s V_id3_tag_detachframe_i
      + (3 # 1) * s V_id3_tag_detachframe_tag_dref_off24
      + s V_id3_tag_detachframe_z
      + (2 # 1) * max0(s V_id3_tag_detachframe_i
                       - s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 31 => ((3 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 32 => ((3 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 33 => ((3 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 34 => ((3 # 1) - s V_id3_tag_detachframe_i
            + s V_id3_tag_detachframe_tag_dref_off24
            + s V_id3_tag_detachframe_z <= z)%Q
   | 35 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 36 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 37 => (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
            + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_id3_tag_detachframe_tag_dref_off24)) (F_check_ge (1
                                                                    + s V_id3_tag_detachframe_tag_dref_off24) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_id3_tag_detachframe_i
                                                 + s V_id3_tag_detachframe_tag_dref_off24)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_id3_tag_detachframe_i
                                                               + s V_id3_tag_detachframe_tag_dref_off24) (0))) (F_max0_ge_0 (1
                                                                    - s V_id3_tag_detachframe_i
                                                                    + s V_id3_tag_detachframe_tag_dref_off24))]
     (-s V_id3_tag_detachframe_i + s V_id3_tag_detachframe_z
      + max0(1 + s V_id3_tag_detachframe_tag_dref_off24) <= z)%Q
   | 39 => (s V_id3_tag_detachframe_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_id3_tag_detachframe =>
    [mkPA Q (fun n z s => ai_id3_tag_detachframe n s /\ annot0_id3_tag_detachframe n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_id3_tag_detachframe (proc_start P_id3_tag_detachframe) s1 (proc_end P_id3_tag_detachframe) s2 ->
    (s2 V_id3_tag_detachframe_z <= max0(1
                                        + s1 V_id3_tag_detachframe_tag_dref_off24))%Q.
Proof.
  prove_bound ipa admissible_ipa P_id3_tag_detachframe.
Qed.
