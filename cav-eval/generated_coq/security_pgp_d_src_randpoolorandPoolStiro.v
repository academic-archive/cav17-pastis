Require Import pasta.Pasta.

Inductive proc: Type :=
  P_randPoolStir.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_randPoolStir_z := 1%positive.
Notation V_randPoolStir_i := 2%positive.
Notation V_randPoolStir_randPoolAddPos := 3%positive.
Notation V_randPoolStir_randPoolGetPos := 4%positive.
Notation V_randPoolStir_t := 5%positive.
Definition Pedges_randPoolStir: list (edge proc) :=
  (EA 1 (AAssign V_randPoolStir_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_randPoolStir_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_randPoolStir_i) s) <
  (eval (ENum (96)) s))%Z)) 48)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_randPoolStir_i) s) >= (eval (ENum (96))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_randPoolStir_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_randPoolStir_i) s) < (eval (ENum (96))
  s))%Z)) 41)::(EA 10 (AGuard (fun s => ((eval (EVar V_randPoolStir_i) s) >=
  (eval (ENum (96)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_randPoolStir_i (Some (ENum (0)))) 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard (fun s => ((eval (EVar V_randPoolStir_i)
  s) < (eval (ENum (96)) s))%Z)) 34)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_randPoolStir_i) s) >= (eval (ENum (96))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign V_randPoolStir_i
  (Some (ENum (0)))) 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_randPoolStir_i) s) <
  (eval (ENum (96)) s))%Z)) 26)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_randPoolStir_i) s) >= (eval (ENum (96))
  s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_randPoolStir_randPoolAddPos (Some (ENum (0)))) 23)::(EA 23 (AAssign
  V_randPoolStir_randPoolGetPos (Some (ENum (128)))) 24)::
  (EA 24 AWeaken 25)::(EA 26 AWeaken 27)::(EA 27 (AAssign V_randPoolStir_t
  None) 28)::(EA 28 ANone 29)::(EA 29 (AAssign V_randPoolStir_i
  (Some (EAdd (EVar V_randPoolStir_i) (ENum (1))))) 30)::(EA 30 ANone 31)::
  (EA 31 ANone 32)::(EA 32 (AAssign V_randPoolStir_z (Some (EAdd (ENum (1))
  (EVar V_randPoolStir_z)))) 33)::(EA 33 AWeaken 20)::(EA 34 AWeaken 35)::
  (EA 35 ANone 36)::(EA 36 (AAssign V_randPoolStir_i
  (Some (EAdd (EVar V_randPoolStir_i) (ENum (4))))) 37)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_randPoolStir_z (Some (EAdd (ENum (1))
  (EVar V_randPoolStir_z)))) 40)::(EA 40 AWeaken 15)::(EA 41 AWeaken 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_randPoolStir_i
  (Some (EAdd (EVar V_randPoolStir_i) (ENum (4))))) 44)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_randPoolStir_z (Some (EAdd (ENum (1))
  (EVar V_randPoolStir_z)))) 47)::(EA 47 AWeaken 10)::(EA 48 AWeaken 49)::
  (EA 49 (AAssign V_randPoolStir_t None) 50)::(EA 50 ANone 51)::
  (EA 51 (AAssign V_randPoolStir_i (Some (EAdd (EVar V_randPoolStir_i)
  (ENum (1))))) 52)::(EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_randPoolStir_z (Some (EAdd (ENum (1)) (EVar V_randPoolStir_z)))) 55)::
  (EA 55 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_randPoolStir => Pedges_randPoolStir
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_randPoolStir => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_randPoolStir (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 3 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 4 => (-1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 5 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0)%Z
   | 6 => (1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0)%Z
   | 7 => (-1 * s V_randPoolStir_i + 96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0)%Z
   | 8 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 9 => (-1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 10 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 11 => (1 * s V_randPoolStir_i + -99 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0)%Z
   | 12 => (-1 * s V_randPoolStir_i + 96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 13 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 14 => (-1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 15 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 16 => (1 * s V_randPoolStir_i + -99 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0)%Z
   | 17 => (-1 * s V_randPoolStir_i + 96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 18 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 19 => (-1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 20 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0)%Z
   | 21 => (1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0)%Z
   | 22 => (-1 * s V_randPoolStir_i + 96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0)%Z
   | 23 => (1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0 /\ 1 * s V_randPoolStir_randPoolAddPos <= 0 /\ -1 * s V_randPoolStir_randPoolAddPos <= 0)%Z
   | 24 => (-1 * s V_randPoolStir_randPoolAddPos <= 0 /\ 1 * s V_randPoolStir_randPoolAddPos <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ 1 * s V_randPoolStir_randPoolGetPos + -128 <= 0 /\ -1 * s V_randPoolStir_randPoolGetPos + 128 <= 0)%Z
   | 25 => (-1 * s V_randPoolStir_randPoolGetPos + 128 <= 0 /\ 1 * s V_randPoolStir_randPoolGetPos + -128 <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 96 <= 0 /\ 1 * s V_randPoolStir_randPoolAddPos <= 0 /\ -1 * s V_randPoolStir_randPoolAddPos <= 0)%Z
   | 26 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 27 => (1 * s V_randPoolStir_i + -95 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 28 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 29 => (1 * s V_randPoolStir_i + -95 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 30 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_i + 1 <= 0)%Z
   | 31 => (-1 * s V_randPoolStir_i + 1 <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 32 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_i + 1 <= 0)%Z
   | 33 => (-1 * s V_randPoolStir_i + 1 <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z + 1 <= 0)%Z
   | 34 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 35 => (1 * s V_randPoolStir_i + -95 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 36 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 37 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 38 => (1 * s V_randPoolStir_i + -99 <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 39 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 40 => (1 * s V_randPoolStir_i + -99 <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ -1 * s V_randPoolStir_z + 1 <= 0)%Z
   | 41 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 42 => (1 * s V_randPoolStir_i + -95 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 43 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 44 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 45 => (1 * s V_randPoolStir_i + -99 <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 46 => (-1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ 1 * s V_randPoolStir_i + -99 <= 0)%Z
   | 47 => (1 * s V_randPoolStir_i + -99 <= 0 /\ -1 * s V_randPoolStir_i + 4 <= 0 /\ -1 * s V_randPoolStir_z + 1 <= 0)%Z
   | 48 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 49 => (1 * s V_randPoolStir_i + -95 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 50 => (-1 * s V_randPoolStir_i <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -95 <= 0)%Z
   | 51 => (1 * s V_randPoolStir_i + -95 <= 0 /\ -1 * s V_randPoolStir_z <= 0 /\ -1 * s V_randPoolStir_i <= 0)%Z
   | 52 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_i + 1 <= 0)%Z
   | 53 => (-1 * s V_randPoolStir_i + 1 <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z <= 0)%Z
   | 54 => (-1 * s V_randPoolStir_z <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_i + 1 <= 0)%Z
   | 55 => (-1 * s V_randPoolStir_i + 1 <= 0 /\ 1 * s V_randPoolStir_i + -96 <= 0 /\ -1 * s V_randPoolStir_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_randPoolStir (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((483 # 2) <= z)%Q
   | 2 => ((483 # 2) + s V_randPoolStir_z <= z)%Q
   | 3 => ((291 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 4 => ((291 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 5 => ((291 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (96 - s V_randPoolStir_i) (92
                                                                    - s V_randPoolStir_i));
      (*-1 0*) F_max0_ge_0 (92 - s V_randPoolStir_i)]
     ((291 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 7 => ((291 # 2) + s V_randPoolStir_z <= z)%Q
   | 8 => ((483 # 4) + s V_randPoolStir_z
           + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 9 => ((483 # 4) + s V_randPoolStir_z
           + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 10 => ((483 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 11 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (99 - s V_randPoolStir_i) (95
                                                                    - s V_randPoolStir_i));
      (*-0.25 0*) F_max0_ge_0 (95 - s V_randPoolStir_i)]
     ((483 # 4) + s V_randPoolStir_z
      + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 12 => ((483 # 4) + s V_randPoolStir_z <= z)%Q
   | 13 => ((96 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 14 => ((96 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 15 => ((96 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 16 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (99 - s V_randPoolStir_i) (98
                                                                    - s V_randPoolStir_i));
      (*-0.25 0*) F_max0_ge_0 (98 - s V_randPoolStir_i)]
     ((96 # 1) + s V_randPoolStir_z + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 17 => ((96 # 1) + s V_randPoolStir_z <= z)%Q
   | 18 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 19 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 20 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 21 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 22 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 23 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (96 - s V_randPoolStir_i)) (F_check_ge (0) (0))]
     (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 25 => (s V_randPoolStir_z <= z)%Q
   | 26 => hints
     [(*-1 1e-12*) F_max0_pre_decrement 1 (96 - s V_randPoolStir_i) (1)]
     (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 27 => ((1 # 1) + s V_randPoolStir_z + max0(95 - s V_randPoolStir_i) <= z)%Q
   | 28 => ((1 # 1) + s V_randPoolStir_z + max0(95 - s V_randPoolStir_i) <= z)%Q
   | 29 => ((1 # 1) + s V_randPoolStir_z + max0(95 - s V_randPoolStir_i) <= z)%Q
   | 30 => ((1 # 1) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 31 => ((1 # 1) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 32 => ((1 # 1) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 33 => (s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 34 => hints
     [(*0 0.25*) F_max0_pre_decrement 1 (99 - s V_randPoolStir_i) (4)]
     ((96 # 1) + s V_randPoolStir_z + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 35 => ((97 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(95 - s V_randPoolStir_i) <= z)%Q
   | 36 => ((97 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(95 - s V_randPoolStir_i) <= z)%Q
   | 37 => ((97 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 38 => ((97 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 39 => ((97 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 40 => ((96 # 1) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 41 => hints
     [(*-0.25 0*) F_max0_pre_decrement 1 (99 - s V_randPoolStir_i) (4)]
     ((483 # 4) + s V_randPoolStir_z
      + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 42 => ((487 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(95 - s V_randPoolStir_i) <= z)%Q
   | 43 => ((487 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(95 - s V_randPoolStir_i) <= z)%Q
   | 44 => ((487 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 45 => ((487 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 46 => ((487 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 47 => ((483 # 4) + s V_randPoolStir_z
            + (1 # 4) * max0(99 - s V_randPoolStir_i) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (96 - s V_randPoolStir_i) (1)]
     ((291 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 49 => ((293 # 2) + s V_randPoolStir_z + max0(95 - s V_randPoolStir_i) <= z)%Q
   | 50 => ((293 # 2) + s V_randPoolStir_z + max0(95 - s V_randPoolStir_i) <= z)%Q
   | 51 => ((293 # 2) + s V_randPoolStir_z + max0(95 - s V_randPoolStir_i) <= z)%Q
   | 52 => ((293 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 53 => ((293 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 54 => ((293 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | 55 => ((291 # 2) + s V_randPoolStir_z + max0(96 - s V_randPoolStir_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_randPoolStir =>
    [mkPA Q (fun n z s => ai_randPoolStir n s /\ annot0_randPoolStir n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_randPoolStir (proc_start P_randPoolStir) s1 (proc_end P_randPoolStir) s2 ->
    (s2 V_randPoolStir_z <= (483 # 2))%Q.
Proof.
  prove_bound ipa admissible_ipa P_randPoolStir.
Qed.
