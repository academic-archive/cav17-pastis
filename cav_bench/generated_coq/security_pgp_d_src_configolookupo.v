Require Import pasta.Pasta.

Inductive proc: Type :=
  P_lookup.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_lookup_z := 1%positive.
Notation V_lookup__tmp := 2%positive.
Notation V_lookup__tmp1 := 3%positive.
Notation V_lookup__tmp2 := 4%positive.
Notation V_lookup_index := 5%positive.
Notation V_lookup_noMatches := 6%positive.
Notation V_lookup_position := 7%positive.
Notation V_lookup_key := 8%positive.
Notation V_lookup_keyLength := 9%positive.
Notation V_lookup_keyWords := 10%positive.
Notation V_lookup_range := 11%positive.
Definition Pedges_lookup: list (edge proc) :=
  (EA 1 (AAssign V_lookup_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_lookup__tmp (Some (EVar V_lookup_keyLength))) 3)::(EA 3 (AAssign
  V_lookup__tmp1 (Some (EVar V_lookup_range))) 4)::(EA 4 (AAssign
  V_lookup_position (Some (ENum (0)))) 5)::(EA 5 (AAssign V_lookup_noMatches
  (Some (ENum (0)))) 6)::(EA 6 (AAssign V_lookup_index
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_lookup_index) s) < (eval (EVar V_lookup__tmp)
  s))%Z)) 42)::(EA 9 (AGuard (fun s => ((eval (EVar V_lookup_index) s) >=
  (eval (EVar V_lookup__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_lookup_index (Some (ENum (0)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard (fun s => ((eval (EVar V_lookup_index)
  s) < (eval (EVar V_lookup__tmp1) s))%Z)) 25)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_lookup_index) s) >= (eval (EVar V_lookup__tmp1)
  s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 21)::(EA 16 ANone 20)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_lookup__tmp2
  (Some (EVar V_lookup_position))) 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 35)::(EA 20 ANone 22)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_lookup__tmp2 (Some (ENum (-1)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 35)::(EA 25 AWeaken 26)::(EA 26 ANone 36)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 28 ANone 32)::(EA 28 ANone 29)::
  (EA 29 (AAssign V_lookup_position (Some (EVar V_lookup_index))) 30)::
  (EA 30 (AAssign V_lookup_noMatches (Some (EAdd (EVar V_lookup_noMatches)
  (ENum (1))))) 31)::(EA 31 ANone 36)::(EA 32 (AAssign V_lookup__tmp2
  (Some (EVar V_lookup_index))) 33)::(EA 33 ANone 34)::(EA 34 AWeaken 35)::
  (EA 36 ANone 37)::(EA 37 (AAssign V_lookup_index
  (Some (EAdd (EVar V_lookup_index) (ENum (1))))) 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_lookup_z (Some (EAdd (ENum (1))
  (EVar V_lookup_z)))) 41)::(EA 41 AWeaken 14)::(EA 42 AWeaken 43)::
  (EA 43 ANone 44)::(EA 44 (AAssign V_lookup_index
  (Some (EAdd (EVar V_lookup_index) (ENum (1))))) 45)::(EA 45 ANone 46)::
  (EA 46 ANone 47)::(EA 47 (AAssign V_lookup_z (Some (EAdd (ENum (1))
  (EVar V_lookup_z)))) 48)::(EA 48 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_lookup => Pedges_lookup
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_lookup => 35
     end)%positive;
  var_global := var_global
}.

Definition ai_lookup (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_z <= 0)%Z
   | 3 => (-1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_z <= 0)%Z
   | 4 => (1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_z <= 0)%Z
   | 5 => (-1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 6 => (-1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0)%Z
   | 7 => (-1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_index <= 0)%Z
   | 8 => (-1 * s V_lookup_index <= 0 /\ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0)%Z
   | 9 => (-1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 10 => (-1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp+ -1 * s V_lookup_index <= 0)%Z
   | 11 => (1 * s V_lookup__tmp+ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 12 => (-1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_index <= 0)%Z
   | 13 => (-1 * s V_lookup_index <= 0 /\ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 14 => (-1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 15 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0)%Z
   | 16 => (1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 17 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0)%Z
   | 18 => (1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup__tmp2 <= 0)%Z
   | 19 => (-1 * s V_lookup__tmp2 <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0)%Z
   | 20 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0)%Z
   | 21 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0)%Z
   | 22 => (1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 23 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0 /\ 1 * s V_lookup__tmp2 + 1 <= 0 /\ -1 * s V_lookup__tmp2 + -1 <= 0)%Z
   | 24 => (-1 * s V_lookup__tmp2 + -1 <= 0 /\ 1 * s V_lookup__tmp2 + 1 <= 0 /\ 1 * s V_lookup__tmp1+ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 25 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 26 => (-1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 27 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 28 => (-1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 29 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 30 => (-1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_position + 1 <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 31 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_position + 1 <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_noMatches + 1 <= 0)%Z
   | 32 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 33 => (-1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup__tmp2 + 1 <= 0 /\ -1 * s V_lookup__tmp2 <= 0)%Z
   | 34 => (-1 * s V_lookup__tmp2 <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup__tmp2 + 1 <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 35 => (-1 * s V_lookup__tmp2 + -1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 36 => (-1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 37 => (-1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_noMatches <= 0)%Z
   | 38 => (-1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index <= 0)%Z
   | 39 => (-1 * s V_lookup__tmp1+ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_noMatches <= 0)%Z
   | 40 => (-1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup__tmp1+ 1 * s V_lookup_index <= 0)%Z
   | 41 => (-1 * s V_lookup__tmp1+ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z + 1 <= 0)%Z
   | 42 => (-1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 43 => (-1 * s V_lookup__tmp+ 1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0)%Z
   | 44 => (-1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_z <= 0 /\ -1 * s V_lookup__tmp+ 1 * s V_lookup_index + 1 <= 0)%Z
   | 45 => (-1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup__tmp+ 1 * s V_lookup_index <= 0)%Z
   | 46 => (-1 * s V_lookup__tmp+ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z <= 0)%Z
   | 47 => (-1 * s V_lookup_z <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ 1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_position <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup__tmp+ 1 * s V_lookup_index <= 0)%Z
   | 48 => (-1 * s V_lookup__tmp+ 1 * s V_lookup_index <= 0 /\ -1 * s V_lookup_index + 1 <= 0 /\ -1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_position <= 0 /\ 1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_noMatches <= 0 /\ -1 * s V_lookup_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_lookup (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_lookup_keyLength) + max0(s V_lookup_range) <= z)%Q
   | 2 => (s V_lookup_z + max0(s V_lookup_keyLength) + max0(s V_lookup_range) <= z)%Q
   | 3 => (s V_lookup_z + max0(s V_lookup__tmp) + max0(s V_lookup_range) <= z)%Q
   | 4 => (s V_lookup_z + max0(s V_lookup__tmp) + max0(s V_lookup__tmp1) <= z)%Q
   | 5 => (s V_lookup_z + max0(s V_lookup__tmp) + max0(s V_lookup__tmp1) <= z)%Q
   | 6 => (s V_lookup_z + max0(s V_lookup__tmp) + max0(s V_lookup__tmp1) <= z)%Q
   | 7 => (s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
           + max0(s V_lookup__tmp1) <= z)%Q
   | 8 => (s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
           + max0(s V_lookup__tmp1) <= z)%Q
   | 9 => (s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
           + max0(s V_lookup__tmp1) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_lookup__tmp
                                             - s V_lookup_index) (-1
                                                                  + s V_lookup__tmp
                                                                  - s V_lookup_index));
      (*0 1*) F_max0_ge_0 (-1 + s V_lookup__tmp - s V_lookup_index)]
     (s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
      + max0(s V_lookup__tmp1) <= z)%Q
   | 11 => (s V_lookup_z + max0(s V_lookup__tmp1) <= z)%Q
   | 12 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 13 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 14 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 15 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 16 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 17 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 18 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_lookup__tmp1
                                             - s V_lookup_index) (-1
                                                                  + s V_lookup__tmp1
                                                                  - s V_lookup_index));
      (*-1 0*) F_max0_ge_0 (-1 + s V_lookup__tmp1 - s V_lookup_index)]
     (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 20 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 21 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 22 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 23 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_lookup__tmp1
                                             - s V_lookup_index) (-1
                                                                  + s V_lookup__tmp1
                                                                  - s V_lookup_index));
      (*-1 0*) F_max0_ge_0 (-1 + s V_lookup__tmp1 - s V_lookup_index)]
     (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_lookup__tmp1 - s V_lookup_index) (1)]
     (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 26 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 27 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 28 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 29 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 30 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 31 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 32 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 33 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_lookup__tmp1 - s V_lookup_index)]
     ((1 # 1) + s V_lookup_z + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 35 => (s V_lookup_z <= z)%Q
   | 36 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 37 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 38 => ((1 # 1) + s V_lookup_z
            + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 39 => ((1 # 1) + s V_lookup_z
            + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 40 => ((1 # 1) + s V_lookup_z
            + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 41 => (s V_lookup_z + max0(s V_lookup__tmp1 - s V_lookup_index) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_lookup__tmp - s V_lookup_index) (1)]
     (s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
      + max0(s V_lookup__tmp1) <= z)%Q
   | 43 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp - s V_lookup_index)
            + max0(s V_lookup__tmp1) <= z)%Q
   | 44 => ((1 # 1) + s V_lookup_z
            + max0(-1 + s V_lookup__tmp - s V_lookup_index)
            + max0(s V_lookup__tmp1) <= z)%Q
   | 45 => ((1 # 1) + s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
            + max0(s V_lookup__tmp1) <= z)%Q
   | 46 => ((1 # 1) + s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
            + max0(s V_lookup__tmp1) <= z)%Q
   | 47 => ((1 # 1) + s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
            + max0(s V_lookup__tmp1) <= z)%Q
   | 48 => (s V_lookup_z + max0(s V_lookup__tmp - s V_lookup_index)
            + max0(s V_lookup__tmp1) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_lookup =>
    [mkPA Q (fun n z s => ai_lookup n s /\ annot0_lookup n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_lookup (proc_start P_lookup) s1 (proc_end P_lookup) s2 ->
    (s2 V_lookup_z <= max0(s1 V_lookup_keyLength) + max0(s1 V_lookup_range))%Q.
Proof.
  prove_bound ipa admissible_ipa P_lookup.
Qed.
