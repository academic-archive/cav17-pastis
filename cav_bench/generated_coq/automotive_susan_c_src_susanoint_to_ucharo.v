Require Import pasta.Pasta.

Inductive proc: Type :=
  P_int_to_uchar.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_int_to_uchar_z := 1%positive.
Notation V_int_to_uchar__tmp := 2%positive.
Notation V_int_to_uchar_i := 3%positive.
Notation V_int_to_uchar_max_r := 4%positive.
Notation V_int_to_uchar_min_r := 5%positive.
Notation V_int_to_uchar_r_dref_off0 := 6%positive.
Notation V_int_to_uchar_in := 7%positive.
Notation V_int_to_uchar_r := 8%positive.
Notation V_int_to_uchar_size := 9%positive.
Definition Pedges_int_to_uchar: list (edge proc) :=
  (EA 1 (AAssign V_int_to_uchar_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_int_to_uchar__tmp (Some (EVar V_int_to_uchar_size))) 3)::(EA 3 (AAssign
  V_int_to_uchar_max_r (Some (EVar V_int_to_uchar_r_dref_off0))) 4)::
  (EA 4 (AAssign V_int_to_uchar_min_r
  (Some (EVar V_int_to_uchar_r_dref_off0))) 5)::(EA 5 (AAssign
  V_int_to_uchar_i (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_int_to_uchar_i) s) <
  (eval (EVar V_int_to_uchar__tmp) s))%Z)) 24)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_int_to_uchar_i) s) >=
  (eval (EVar V_int_to_uchar__tmp) s))%Z)) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_int_to_uchar_max_r
  (Some (ESub (EVar V_int_to_uchar_max_r)
  (EVar V_int_to_uchar_min_r)))) 11)::(EA 11 (AAssign V_int_to_uchar_i
  (Some (ENum (0)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_int_to_uchar_i) s) <
  (eval (EVar V_int_to_uchar__tmp) s))%Z)) 17)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_int_to_uchar_i) s) >=
  (eval (EVar V_int_to_uchar__tmp) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_int_to_uchar_i
  (Some (EAdd (EVar V_int_to_uchar_i) (ENum (1))))) 20)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_int_to_uchar_z (Some (EAdd (ENum (1))
  (EVar V_int_to_uchar_z)))) 23)::(EA 23 AWeaken 14)::(EA 24 AWeaken 25)::
  (EA 25 ANone 27)::(EA 25 ANone 26)::(EA 26 AWeaken 30)::(EA 27 (AAssign
  V_int_to_uchar_max_r None) 28)::(EA 28 ANone 29)::(EA 29 AWeaken 30)::
  (EA 30 ANone 31)::(EA 30 ANone 33)::(EA 31 (AAssign V_int_to_uchar_min_r
  None) 32)::(EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_int_to_uchar_i (Some (EAdd (EVar V_int_to_uchar_i) (ENum (1))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_int_to_uchar_z
  (Some (EAdd (ENum (1)) (EVar V_int_to_uchar_z)))) 38)::(EA 38 AWeaken 8)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_int_to_uchar => Pedges_int_to_uchar
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_int_to_uchar => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_int_to_uchar (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_z <= 0)%Z
   | 3 => (-1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar_z <= 0)%Z
   | 4 => (1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_z <= 0)%Z
   | 5 => (-1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar_z <= 0)%Z
   | 6 => (1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 7 => (-1 * s V_int_to_uchar_i <= 0 /\ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar_z <= 0)%Z
   | 8 => (-1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 9 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar__tmp+ -1 * s V_int_to_uchar_i <= 0)%Z
   | 10 => (1 * s V_int_to_uchar__tmp+ -1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 11 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar__tmp+ -1 * s V_int_to_uchar_i <= 0)%Z
   | 12 => (-1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 13 => (-1 * s V_int_to_uchar_i <= 0 /\ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0)%Z
   | 14 => (-1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 15 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ 1 * s V_int_to_uchar__tmp+ -1 * s V_int_to_uchar_i <= 0)%Z
   | 16 => (1 * s V_int_to_uchar__tmp+ -1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 17 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 18 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 19 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 20 => (-1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0)%Z
   | 21 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0)%Z
   | 22 => (-1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0)%Z
   | 23 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z + 1 <= 0)%Z
   | 24 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 25 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 26 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 27 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 28 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 29 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 30 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 31 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 32 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 33 => (-1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 34 => (-1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar_i <= 0)%Z
   | 35 => (-1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 36 => (-1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z <= 0)%Z
   | 37 => (-1 * s V_int_to_uchar_z <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_i + 1 <= 0)%Z
   | 38 => (-1 * s V_int_to_uchar_i + 1 <= 0 /\ -1 * s V_int_to_uchar__tmp+ 1 * s V_int_to_uchar_i <= 0 /\ -1 * s V_int_to_uchar_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_int_to_uchar (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) * max0(s V_int_to_uchar_size) <= z)%Q
   | 2 => (s V_int_to_uchar_z + (2 # 1) * max0(s V_int_to_uchar_size) <= z)%Q
   | 3 => (s V_int_to_uchar_z + (2 # 1) * max0(s V_int_to_uchar__tmp) <= z)%Q
   | 4 => (s V_int_to_uchar_z + (2 # 1) * max0(s V_int_to_uchar__tmp) <= z)%Q
   | 5 => (s V_int_to_uchar_z + (2 # 1) * max0(s V_int_to_uchar__tmp) <= z)%Q
   | 6 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
           + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 7 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
           + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 8 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
           + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_int_to_uchar__tmp
                                             - s V_int_to_uchar_i) (-1
                                                                    + 
                                                                    s V_int_to_uchar__tmp
                                                                    - 
                                                                    s V_int_to_uchar_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)]
     (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
      + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 10 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 11 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 12 => (s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 13 => (s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 14 => (s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_int_to_uchar__tmp
                                             - s V_int_to_uchar_i) (-1
                                                                    + 
                                                                    s V_int_to_uchar__tmp
                                                                    - 
                                                                    s V_int_to_uchar_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)]
     (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 16 => (s V_int_to_uchar_z <= z)%Q
   | 17 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_int_to_uchar__tmp
                                      - s V_int_to_uchar_i) (1)]
     (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 18 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 19 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 20 => ((1 # 1) + s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 21 => ((1 # 1) + s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 22 => ((1 # 1) + s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 23 => (s V_int_to_uchar_z
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 24 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 25 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_int_to_uchar__tmp
                                       - s V_int_to_uchar_i) (1)]
     (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
      + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 27 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 28 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_int_to_uchar__tmp
                                       - s V_int_to_uchar_i) (1)]
     (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
      + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 30 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)
            + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 31 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)
            + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 32 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)
            + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 33 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)
            + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 34 => ((1 # 1) + s V_int_to_uchar_z
            + max0(-1 + s V_int_to_uchar__tmp - s V_int_to_uchar_i)
            + max0(s V_int_to_uchar__tmp) <= z)%Q
   | 35 => ((1 # 1) + s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 36 => ((1 # 1) + s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 37 => ((1 # 1) + s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | 38 => (s V_int_to_uchar_z + max0(s V_int_to_uchar__tmp)
            + max0(s V_int_to_uchar__tmp - s V_int_to_uchar_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_int_to_uchar =>
    [mkPA Q (fun n z s => ai_int_to_uchar n s /\ annot0_int_to_uchar n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_int_to_uchar (proc_start P_int_to_uchar) s1 (proc_end P_int_to_uchar) s2 ->
    (s2 V_int_to_uchar_z <= (2 # 1) * max0(s1 V_int_to_uchar_size))%Q.
Proof.
  prove_bound ipa admissible_ipa P_int_to_uchar.
Qed.
