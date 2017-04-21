Require Import pasta.Pasta.

Inductive proc: Type :=
  P_II_samples.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_II_samples_z := 1%positive.
Notation V_II_samples_c := 2%positive.
Notation V_II_samples_nb := 3%positive.
Notation V_II_samples_nlevels := 4%positive.
Notation V_II_samples_quantclass_dref_off0 := 5%positive.
Notation V_II_samples_quantclass_dref_off2 := 6%positive.
Notation V_II_samples_quantclass_dref_off3 := 7%positive.
Notation V_II_samples_requantized := 8%positive.
Notation V_II_samples_s := 9%positive.
Notation V_II_samples_output := 10%positive.
Notation V_II_samples_ptr := 11%positive.
Notation V_II_samples_quantclass := 12%positive.
Definition Pedges_II_samples: list (edge proc) :=
  (EA 1 (AAssign V_II_samples_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_II_samples_s) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_II_samples_nb
  (Some (EVar V_II_samples_quantclass_dref_off2))) 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_II_samples_quantclass_dref_off2)
  s) <> (eval (ENum (0)) s))%Z)) 22)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_II_samples_quantclass_dref_off2) s) =
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_II_samples_nb (Some (EVar V_II_samples_quantclass_dref_off3))) 9)::
  (EA 9 (AAssign V_II_samples_s (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_II_samples_s)
  s) < (eval (ENum (3)) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_II_samples_s) s) >= (eval (ENum (3)) s))%Z)) 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 31)::(EA 15 AWeaken 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_II_samples_s
  (Some (EAdd (EVar V_II_samples_s) (ENum (1))))) 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_II_samples_z (Some (EAdd (ENum (1))
  (EVar V_II_samples_z)))) 21)::(EA 21 AWeaken 12)::(EA 22 AWeaken 23)::
  (EA 23 (AAssign V_II_samples_c None) 24)::(EA 24 (AAssign
  V_II_samples_nlevels (Some (EVar V_II_samples_quantclass_dref_off0))) 25)::
  (EA 25 (AAssign V_II_samples_s (Some (ENum (0)))) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 (AGuard (fun s => ((eval (EVar V_II_samples_s)
  s) < (eval (ENum (3)) s))%Z)) 47)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_II_samples_s) s) >= (eval (ENum (3)) s))%Z)) 29)::
  (EA 29 AWeaken 30)::(EA 30 ANone 31)::(EA 31 (AAssign V_II_samples_s
  (Some (ENum (0)))) 32)::(EA 32 ANone 33)::(EA 33 AWeaken 34)::
  (EA 34 (AGuard (fun s => ((eval (EVar V_II_samples_s) s) < (eval (ENum (3))
  s))%Z)) 37)::(EA 34 (AGuard (fun s => ((eval (EVar V_II_samples_s) s) >=
  (eval (ENum (3)) s))%Z)) 35)::(EA 35 AWeaken 36)::(EA 37 AWeaken 38)::
  (EA 38 (AAssign V_II_samples_requantized None) 39)::(EA 39 (AAssign
  V_II_samples_requantized None) 40)::(EA 40 (AAssign
  V_II_samples_requantized None) 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_II_samples_s (Some (EAdd (EVar V_II_samples_s) (ENum (1))))) 43)::
  (EA 43 ANone 44)::(EA 44 ANone 45)::(EA 45 (AAssign V_II_samples_z
  (Some (EAdd (ENum (1)) (EVar V_II_samples_z)))) 46)::(EA 46 AWeaken 34)::
  (EA 47 AWeaken 48)::(EA 48 (AAssign V_II_samples_c None) 49)::
  (EA 49 ANone 50)::(EA 50 (AAssign V_II_samples_s
  (Some (EAdd (EVar V_II_samples_s) (ENum (1))))) 51)::(EA 51 ANone 52)::
  (EA 52 ANone 53)::(EA 53 (AAssign V_II_samples_z (Some (EAdd (ENum (1))
  (EVar V_II_samples_z)))) 54)::(EA 54 AWeaken 28)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_II_samples => Pedges_II_samples
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_II_samples => 36
     end)%positive;
  var_global := var_global
}.

Definition ai_II_samples (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 3 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 4 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 5 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 6 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 7 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0)%Z
   | 8 => (-1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 9 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0)%Z
   | 10 => (-1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 11 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0)%Z
   | 12 => (-1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 13 => (1 * s V_II_samples_s + -3 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s + 3 <= 0)%Z
   | 14 => (-1 * s V_II_samples_s + 3 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 15 => (1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 16 => (1 * s V_II_samples_s + -2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0)%Z
   | 17 => (1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 18 => (-1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 19 => (1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 20 => (-1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 21 => (1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_quantclass_dref_off2 <= 0 /\ -1 * s V_II_samples_z + 1 <= 0)%Z
   | 22 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 23 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 24 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 25 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 26 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 27 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 28 => (-1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 29 => (1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s + 3 <= 0)%Z
   | 30 => (-1 * s V_II_samples_s + 3 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 31 => (1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s + 3 <= 0)%Z
   | 32 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 33 => (-1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 34 => (-1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 35 => (1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s + 3 <= 0)%Z
   | 36 => (-1 * s V_II_samples_s + 3 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -3 <= 0)%Z
   | 37 => (-1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 38 => (1 * s V_II_samples_s + -2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 39 => (-1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 40 => (1 * s V_II_samples_s + -2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 41 => (-1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 42 => (1 * s V_II_samples_s + -2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 43 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0)%Z
   | 44 => (-1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 45 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0)%Z
   | 46 => (-1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z + 1 <= 0)%Z
   | 47 => (-1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 48 => (1 * s V_II_samples_s + -2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 49 => (-1 * s V_II_samples_s <= 0 /\ -1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -2 <= 0)%Z
   | 50 => (1 * s V_II_samples_s + -2 <= 0 /\ -1 * s V_II_samples_z <= 0 /\ -1 * s V_II_samples_s <= 0)%Z
   | 51 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0)%Z
   | 52 => (-1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z <= 0)%Z
   | 53 => (-1 * s V_II_samples_z <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_s + 1 <= 0)%Z
   | 54 => (-1 * s V_II_samples_s + 1 <= 0 /\ 1 * s V_II_samples_s + -3 <= 0 /\ -1 * s V_II_samples_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_II_samples (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((6 # 1) <= z)%Q
   | 2 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 3 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 4 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 5 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 6 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 7 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 8 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 9 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 10 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 11 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 12 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_II_samples_s) (2
                                                                    - 
                                                                    s V_II_samples_s));
      (*-1 0*) F_max0_ge_0 (2 - s V_II_samples_s)]
     ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 14 => ((3 # 1) + s V_II_samples_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_II_samples_s) (1)]
     ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 16 => ((4 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 17 => ((4 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 18 => ((4 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 19 => ((4 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 20 => ((4 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 21 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 22 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 23 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 24 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 25 => ((6 # 1) + s V_II_samples_z <= z)%Q
   | 26 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 27 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 28 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_II_samples_s) (2
                                                                    - 
                                                                    s V_II_samples_s));
      (*-1 0*) F_max0_ge_0 (2 - s V_II_samples_s)]
     ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 30 => ((3 # 1) + s V_II_samples_z <= z)%Q
   | 31 => ((3 # 1) + s V_II_samples_z <= z)%Q
   | 32 => (s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 33 => (s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 34 => (s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_II_samples_s) (2
                                                                    - 
                                                                    s V_II_samples_s));
      (*-1 0*) F_max0_ge_0 (2 - s V_II_samples_s)]
     (s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 36 => (s V_II_samples_z <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_II_samples_s) (1)]
     (s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 38 => ((1 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 39 => ((1 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 40 => ((1 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 41 => ((1 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 42 => ((1 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 43 => ((1 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 44 => ((1 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 45 => ((1 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 46 => (s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_II_samples_s) (1)]
     ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 48 => ((4 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 49 => ((4 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 50 => ((4 # 1) + s V_II_samples_z + max0(2 - s V_II_samples_s) <= z)%Q
   | 51 => ((4 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 52 => ((4 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 53 => ((4 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | 54 => ((3 # 1) + s V_II_samples_z + max0(3 - s V_II_samples_s) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_II_samples =>
    [mkPA Q (fun n z s => ai_II_samples n s /\ annot0_II_samples n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_II_samples (proc_start P_II_samples) s1 (proc_end P_II_samples) s2 ->
    (s2 V_II_samples_z <= (6 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_II_samples.
Qed.
