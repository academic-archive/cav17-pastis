Require Import pasta.Pasta.

Inductive proc: Type :=
  P_LARp_to_rp.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_LARp_to_rp_z := 1%positive.
Notation V_LARp_to_rp_i := 2%positive.
Notation V_LARp_to_rp_ltmp := 3%positive.
Notation V_LARp_to_rp_temp := 4%positive.
Notation V_LARp_to_rp_LARp := 5%positive.
Definition Pedges_LARp_to_rp: list (edge proc) :=
  (EA 1 (AAssign V_LARp_to_rp_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_LARp_to_rp_i (Some (ENum (1)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_LARp_to_rp_i) s) <= (eval (ENum (8))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_LARp_to_rp_i) s) >
  (eval (ENum (8)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 ANone 25)::(EA 9 ANone 10)::(EA 10 (AAssign V_LARp_to_rp_temp
  None) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 23)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 21)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_LARp_to_rp_ltmp None) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 19)::
  (EA 17 ANone 18)::(EA 18 ANone 20)::(EA 19 ANone 20)::(EA 20 ANone 22)::
  (EA 21 ANone 22)::(EA 22 ANone 24)::(EA 23 ANone 24)::(EA 24 ANone 45)::
  (EA 25 AWeaken 26)::(EA 26 ANone 42)::(EA 26 ANone 27)::(EA 27 ANone 28)::
  (EA 28 (AAssign V_LARp_to_rp_temp None) 29)::(EA 29 AWeaken 30)::
  (EA 30 (AGuard (fun s => True)) 41)::(EA 30 (AGuard (fun s => True)) 31)::
  (EA 31 AWeaken 32)::(EA 32 ANone 39)::(EA 32 ANone 33)::(EA 33 (AAssign
  V_LARp_to_rp_ltmp None) 34)::(EA 34 AWeaken 35)::(EA 35 ANone 37)::
  (EA 35 ANone 36)::(EA 36 ANone 38)::(EA 37 ANone 38)::(EA 38 ANone 40)::
  (EA 39 ANone 40)::(EA 40 ANone 44)::(EA 41 AWeaken 43)::(EA 42 ANone 43)::
  (EA 43 ANone 44)::(EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 (AAssign
  V_LARp_to_rp_i (Some (EAdd (EVar V_LARp_to_rp_i) (ENum (1))))) 47)::
  (EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 (AAssign V_LARp_to_rp_z
  (Some (EAdd (ENum (1)) (EVar V_LARp_to_rp_z)))) 50)::(EA 50 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_LARp_to_rp => Pedges_LARp_to_rp
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_LARp_to_rp => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_LARp_to_rp (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_z <= 0)%Z
   | 3 => (-1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -1 <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 4 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ 1 * s V_LARp_to_rp_i + -1 <= 0 /\ 1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_z <= 0)%Z
   | 5 => (-1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0 /\ 1 * s V_LARp_to_rp_i + -9 <= 0)%Z
   | 6 => (1 * s V_LARp_to_rp_i + -9 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 9 <= 0)%Z
   | 7 => (-1 * s V_LARp_to_rp_i + 9 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -9 <= 0)%Z
   | 8 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 9 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 10 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 11 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 12 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 13 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 14 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 15 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 16 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 17 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 18 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 19 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 20 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 21 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 22 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 23 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 24 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 25 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 26 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 27 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 28 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 29 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 30 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 31 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 32 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 33 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 34 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 35 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 36 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 37 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 38 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 39 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 40 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 41 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 42 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 43 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 44 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 45 => (1 * s V_LARp_to_rp_i + -8 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 1 <= 0)%Z
   | 46 => (-1 * s V_LARp_to_rp_i + 1 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0 /\ 1 * s V_LARp_to_rp_i + -8 <= 0)%Z
   | 47 => (-1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 2 <= 0 /\ 1 * s V_LARp_to_rp_i + -9 <= 0)%Z
   | 48 => (1 * s V_LARp_to_rp_i + -9 <= 0 /\ -1 * s V_LARp_to_rp_i + 2 <= 0 /\ -1 * s V_LARp_to_rp_z <= 0)%Z
   | 49 => (-1 * s V_LARp_to_rp_z <= 0 /\ -1 * s V_LARp_to_rp_i + 2 <= 0 /\ 1 * s V_LARp_to_rp_i + -9 <= 0)%Z
   | 50 => (1 * s V_LARp_to_rp_i + -9 <= 0 /\ -1 * s V_LARp_to_rp_i + 2 <= 0 /\ -1 * s V_LARp_to_rp_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_LARp_to_rp (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_LARp_to_rp_z <= z)%Q
   | 3 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 4 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 5 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (9 - s V_LARp_to_rp_i) (8
                                                                    - 
                                                                    s V_LARp_to_rp_i));
      (*-1 0*) F_max0_ge_0 (8 - s V_LARp_to_rp_i)]
     (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 7 => (s V_LARp_to_rp_z <= z)%Q
   | 8 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 9 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 10 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_LARp_to_rp_i) (1)]
     (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 12 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 13 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 14 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 15 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 16 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 17 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 18 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 19 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 20 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 21 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 22 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 23 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 24 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_LARp_to_rp_i) (1)]
     (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 26 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 27 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 28 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 29 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 30 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 31 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 32 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 33 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 34 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 35 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 36 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 37 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 38 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 39 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 40 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 41 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 42 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 43 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 44 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 45 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 46 => ((1 # 1) + s V_LARp_to_rp_z + max0(8 - s V_LARp_to_rp_i) <= z)%Q
   | 47 => ((1 # 1) + s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 48 => ((1 # 1) + s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 49 => ((1 # 1) + s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | 50 => (s V_LARp_to_rp_z + max0(9 - s V_LARp_to_rp_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_LARp_to_rp =>
    [mkPA Q (fun n z s => ai_LARp_to_rp n s /\ annot0_LARp_to_rp n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_LARp_to_rp (proc_start P_LARp_to_rp) s1 (proc_end P_LARp_to_rp) s2 ->
    (s2 V_LARp_to_rp_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_LARp_to_rp.
Qed.
