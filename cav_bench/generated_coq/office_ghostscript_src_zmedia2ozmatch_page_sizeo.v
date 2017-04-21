Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zmatch_page_size.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zmatch_page_size_z := 1%positive.
Notation V_zmatch_page_size__tmp := 2%positive.
Notation V_zmatch_page_size__tmp1 := 3%positive.
Notation V_zmatch_page_size__tmp2 := 4%positive.
Notation V_zmatch_page_size__tmp3 := 5%positive.
Notation V_zmatch_page_size_code := 6%positive.
Notation V_zmatch_page_size_i := 7%positive.
Notation V_zmatch_page_size_nm := 8%positive.
Notation V_zmatch_page_size_best_mismatch := 9%positive.
Notation V_zmatch_page_size_orient := 10%positive.
Notation V_zmatch_page_size_pmat := 11%positive.
Notation V_zmatch_page_size_pmsize := 12%positive.
Notation V_zmatch_page_size_policy := 13%positive.
Notation V_zmatch_page_size_pvmed := 14%positive.
Notation V_zmatch_page_size_pvreq := 15%positive.
Notation V_zmatch_page_size_roll := 16%positive.
Definition Pedges_zmatch_page_size: list (edge proc) :=
  (EA 1 (AAssign V_zmatch_page_size_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_zmatch_page_size_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_zmatch_page_size__tmp3
  (Some (EVar V_zmatch_page_size_policy))) 5)::(EA 5 (AAssign
  V_zmatch_page_size__tmp2 (Some (EVar V_zmatch_page_size_orient))) 6)::
  (EA 6 (AAssign V_zmatch_page_size__tmp1
  (Some (EVar V_zmatch_page_size_roll))) 7)::(EA 7 AWeaken 8)::
  (EA 8 ANone 12)::(EA 8 ANone 9)::(EA 9 (AAssign V_zmatch_page_size__tmp
  None) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 43)::(EA 12 AWeaken 13)::
  (EA 13 ANone 17)::(EA 13 ANone 14)::(EA 14 (AAssign V_zmatch_page_size__tmp
  None) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 43)::(EA 17 AWeaken 18)::
  (EA 18 ANone 19)::(EA 18 ANone 25)::(EA 19 (AAssign V_zmatch_page_size_nm
  None) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 29)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_zmatch_page_size_nm) s) = (eval (ENum (4))
  s))%Z)) 28)::(EA 23 (AGuard (fun s => ((eval (EVar V_zmatch_page_size_nm)
  s) <> (eval (ENum (4)) s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 (AAssign
  V_zmatch_page_size__tmp (Some (ENum (-15)))) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 43)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_zmatch_page_size_i (Some (ENum (0)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_zmatch_page_size_i) s) < (eval (ENum (4))
  s))%Z)) 44)::(EA 32 (AGuard (fun s => ((eval (EVar V_zmatch_page_size_i)
  s) >= (eval (ENum (4)) s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 (AAssign
  V_zmatch_page_size_code None) 35)::(EA 35 AWeaken 36)::(EA 36 ANone 40)::
  (EA 36 ANone 37)::(EA 37 (AAssign V_zmatch_page_size__tmp None) 38)::
  (EA 38 ANone 39)::(EA 39 AWeaken 43)::(EA 40 (AAssign
  V_zmatch_page_size__tmp (Some (EVar V_zmatch_page_size_code))) 41)::
  (EA 41 ANone 42)::(EA 42 AWeaken 43)::(EA 44 AWeaken 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_zmatch_page_size_i
  (Some (EAdd (EVar V_zmatch_page_size_i) (ENum (1))))) 47)::
  (EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 (AAssign V_zmatch_page_size_z
  (Some (EAdd (ENum (1)) (EVar V_zmatch_page_size_z)))) 50)::
  (EA 50 AWeaken 32)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zmatch_page_size => Pedges_zmatch_page_size
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zmatch_page_size => 43
     end)%positive;
  var_global := var_global
}.

Definition ai_zmatch_page_size (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 3 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 4 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 5 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 6 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 7 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 8 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 9 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 10 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 11 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 12 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 13 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 14 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 15 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 16 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 17 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 18 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 19 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 20 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 21 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 22 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 23 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 24 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 25 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 26 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size__tmp + 15 <= 0 /\ -1 * s V_zmatch_page_size__tmp + -15 <= 0)%Z
   | 27 => (-1 * s V_zmatch_page_size__tmp + -15 <= 0 /\ 1 * s V_zmatch_page_size__tmp + 15 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 28 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_nm + -4 <= 0 /\ -1 * s V_zmatch_page_size_nm + 4 <= 0)%Z
   | 29 => (-1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 30 => (1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 31 => (-1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_i <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_z <= 0)%Z
   | 32 => (-1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 33 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 4 <= 0)%Z
   | 34 => (-1 * s V_zmatch_page_size_i + 4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 35 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 4 <= 0)%Z
   | 36 => (-1 * s V_zmatch_page_size_i + 4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 37 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 4 <= 0)%Z
   | 38 => (-1 * s V_zmatch_page_size_i + 4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 39 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 4 <= 0)%Z
   | 40 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 4 <= 0)%Z
   | 41 => (-1 * s V_zmatch_page_size_i + 4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 42 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 4 <= 0)%Z
   | 43 => (-1 * s V_zmatch_page_size_i <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 44 => (-1 * s V_zmatch_page_size_i <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i + -3 <= 0)%Z
   | 45 => (1 * s V_zmatch_page_size_i + -3 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i <= 0)%Z
   | 46 => (-1 * s V_zmatch_page_size_i <= 0 /\ -1 * s V_zmatch_page_size_z <= 0 /\ 1 * s V_zmatch_page_size_i + -3 <= 0)%Z
   | 47 => (-1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 1 <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 48 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_i + 1 <= 0 /\ -1 * s V_zmatch_page_size_z <= 0)%Z
   | 49 => (-1 * s V_zmatch_page_size_z <= 0 /\ -1 * s V_zmatch_page_size_i + 1 <= 0 /\ 1 * s V_zmatch_page_size_i + -4 <= 0)%Z
   | 50 => (1 * s V_zmatch_page_size_i + -4 <= 0 /\ -1 * s V_zmatch_page_size_i + 1 <= 0 /\ -1 * s V_zmatch_page_size_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zmatch_page_size (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 3 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 4 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 5 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 6 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 7 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 8 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 9 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 10 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 11 => hints
     [(*-4 0*) F_one]
     ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 12 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 13 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 14 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 15 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 16 => hints
     [(*-4 0*) F_one]
     ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 17 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 18 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 19 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zmatch_page_size_z)) (F_check_ge (s V_zmatch_page_size_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zmatch_page_size_z) (0))) (F_max0_ge_0 (s V_zmatch_page_size_z))]
     ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 21 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 22 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 23 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 24 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 25 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 26 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 27 => hints
     [(*-4 0*) F_one]
     ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 28 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 29 => ((4 # 1) + s V_zmatch_page_size_z <= z)%Q
   | 30 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 31 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 32 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 33 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 34 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 35 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 36 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 37 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 38 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 39 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_zmatch_page_size_i) (3
                                                                    - s V_zmatch_page_size_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_zmatch_page_size_i)]
     (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 40 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 41 => (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_zmatch_page_size_i) (3
                                                                    - s V_zmatch_page_size_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_zmatch_page_size_i)]
     (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 43 => (s V_zmatch_page_size_z <= z)%Q
   | 44 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4 - s V_zmatch_page_size_i)) (F_check_ge (4
                                                                    - s V_zmatch_page_size_i) (0))]
     (s V_zmatch_page_size_z + max0(4 - s V_zmatch_page_size_i) <= z)%Q
   | 45 => ((4 # 1) - s V_zmatch_page_size_i + s V_zmatch_page_size_z <= z)%Q
   | 46 => ((4 # 1) - s V_zmatch_page_size_i + s V_zmatch_page_size_z <= z)%Q
   | 47 => ((5 # 1) - s V_zmatch_page_size_i + s V_zmatch_page_size_z <= z)%Q
   | 48 => ((5 # 1) - s V_zmatch_page_size_i + s V_zmatch_page_size_z <= z)%Q
   | 49 => ((5 # 1) - s V_zmatch_page_size_i + s V_zmatch_page_size_z <= z)%Q
   | 50 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_zmatch_page_size_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_zmatch_page_size_i))]
     ((4 # 1) - s V_zmatch_page_size_i + s V_zmatch_page_size_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zmatch_page_size =>
    [mkPA Q (fun n z s => ai_zmatch_page_size n s /\ annot0_zmatch_page_size n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zmatch_page_size (proc_start P_zmatch_page_size) s1 (proc_end P_zmatch_page_size) s2 ->
    (s2 V_zmatch_page_size_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zmatch_page_size.
Qed.
