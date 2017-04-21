Require Import pasta.Pasta.

Inductive proc: Type :=
  P_id3_crc_calculate.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_id3_crc_calculate_z := 1%positive.
Notation V_id3_crc_calculate__tmp := 2%positive.
Notation V_id3_crc_calculate_crc := 3%positive.
Notation V_id3_crc_calculate_data := 4%positive.
Notation V_id3_crc_calculate_length := 5%positive.
Definition Pedges_id3_crc_calculate: list (edge proc) :=
  (EA 1 (AAssign V_id3_crc_calculate_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_id3_crc_calculate__tmp) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_id3_crc_calculate__tmp
  (Some (EVar V_id3_crc_calculate_length))) 5)::(EA 5 (AAssign
  V_id3_crc_calculate_crc None) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_id3_crc_calculate__tmp) s) >=
  (eval (ENum (8)) s))%Z)) 29)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_id3_crc_calculate__tmp) s) < (eval (ENum (8))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 27)::(EA 10 ANone 11)::
  (EA 10 ANone 13)::(EA 10 ANone 15)::(EA 10 ANone 17)::(EA 10 ANone 19)::
  (EA 10 ANone 21)::(EA 10 ANone 23)::(EA 10 ANone 25)::(EA 11 (AAssign
  V_id3_crc_calculate_crc None) 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_id3_crc_calculate_crc None) 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_id3_crc_calculate_crc None) 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_id3_crc_calculate_crc None) 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_id3_crc_calculate_crc None) 20)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_id3_crc_calculate_crc None) 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_id3_crc_calculate_crc None) 24)::(EA 24 ANone 25)::(EA 25 ANone 26)::
  (EA 26 AWeaken 28)::(EA 27 AWeaken 28)::(EA 29 AWeaken 30)::(EA 30 (AAssign
  V_id3_crc_calculate_crc None) 31)::(EA 31 (AAssign V_id3_crc_calculate_crc
  None) 32)::(EA 32 (AAssign V_id3_crc_calculate_crc None) 33)::
  (EA 33 (AAssign V_id3_crc_calculate_crc None) 34)::(EA 34 (AAssign
  V_id3_crc_calculate_crc None) 35)::(EA 35 (AAssign V_id3_crc_calculate_crc
  None) 36)::(EA 36 (AAssign V_id3_crc_calculate_crc None) 37)::
  (EA 37 (AAssign V_id3_crc_calculate_crc None) 38)::(EA 38 ANone 39)::
  (EA 39 (AAssign V_id3_crc_calculate__tmp
  (Some (ESub (EVar V_id3_crc_calculate__tmp) (ENum (8))))) 40)::
  (EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign V_id3_crc_calculate_z
  (Some (EAdd (ENum (1)) (EVar V_id3_crc_calculate_z)))) 43)::
  (EA 43 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_id3_crc_calculate => Pedges_id3_crc_calculate
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_id3_crc_calculate => 28
     end)%positive;
  var_global := var_global
}.

Definition ai_id3_crc_calculate (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 3 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp <= 0)%Z
   | 4 => (-1 * s V_id3_crc_calculate__tmp <= 0 /\ 1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 5 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate_z <= 0)%Z
   | 6 => (1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 7 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate_z <= 0)%Z
   | 8 => (-1 * s V_id3_crc_calculate_z <= 0)%Z
   | 9 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 10 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 11 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 12 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 13 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 14 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 15 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 16 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 17 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 18 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 19 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 20 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 21 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 22 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 23 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 24 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 25 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 26 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 27 => (-1 * s V_id3_crc_calculate_z <= 0 /\ 1 * s V_id3_crc_calculate__tmp + -7 <= 0)%Z
   | 28 => (1 * s V_id3_crc_calculate__tmp + -7 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 29 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp + 8 <= 0)%Z
   | 30 => (-1 * s V_id3_crc_calculate__tmp + 8 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 31 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp + 8 <= 0)%Z
   | 32 => (-1 * s V_id3_crc_calculate__tmp + 8 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 33 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp + 8 <= 0)%Z
   | 34 => (-1 * s V_id3_crc_calculate__tmp + 8 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 35 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp + 8 <= 0)%Z
   | 36 => (-1 * s V_id3_crc_calculate__tmp + 8 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 37 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp + 8 <= 0)%Z
   | 38 => (-1 * s V_id3_crc_calculate__tmp + 8 <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 39 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp + 8 <= 0)%Z
   | 40 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp <= 0)%Z
   | 41 => (-1 * s V_id3_crc_calculate__tmp <= 0 /\ -1 * s V_id3_crc_calculate_z <= 0)%Z
   | 42 => (-1 * s V_id3_crc_calculate_z <= 0 /\ -1 * s V_id3_crc_calculate__tmp <= 0)%Z
   | 43 => (-1 * s V_id3_crc_calculate__tmp <= 0 /\ -1 * s V_id3_crc_calculate_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_id3_crc_calculate (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(s V_id3_crc_calculate_length) <= z)%Q
   | 2 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate_length) <= z)%Q
   | 3 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate_length) <= z)%Q
   | 4 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate_length) <= z)%Q
   | 5 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 6 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 7 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 8 => (s V_id3_crc_calculate_z
           + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 9 => hints
     [(*0 0.125*) F_max0_ge_0 (s V_id3_crc_calculate__tmp)]
     (s V_id3_crc_calculate_z + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 10 => (s V_id3_crc_calculate_z <= z)%Q
   | 11 => (s V_id3_crc_calculate_z <= z)%Q
   | 12 => (s V_id3_crc_calculate_z <= z)%Q
   | 13 => (s V_id3_crc_calculate_z <= z)%Q
   | 14 => (s V_id3_crc_calculate_z <= z)%Q
   | 15 => (s V_id3_crc_calculate_z <= z)%Q
   | 16 => (s V_id3_crc_calculate_z <= z)%Q
   | 17 => (s V_id3_crc_calculate_z <= z)%Q
   | 18 => (s V_id3_crc_calculate_z <= z)%Q
   | 19 => (s V_id3_crc_calculate_z <= z)%Q
   | 20 => (s V_id3_crc_calculate_z <= z)%Q
   | 21 => (s V_id3_crc_calculate_z <= z)%Q
   | 22 => (s V_id3_crc_calculate_z <= z)%Q
   | 23 => (s V_id3_crc_calculate_z <= z)%Q
   | 24 => (s V_id3_crc_calculate_z <= z)%Q
   | 25 => (s V_id3_crc_calculate_z <= z)%Q
   | 26 => (s V_id3_crc_calculate_z <= z)%Q
   | 27 => (s V_id3_crc_calculate_z <= z)%Q
   | 28 => (s V_id3_crc_calculate_z <= z)%Q
   | 29 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (s V_id3_crc_calculate__tmp) (8)]
     (s V_id3_crc_calculate_z + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 30 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 31 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 32 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 33 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 34 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 35 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 36 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 37 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 38 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 39 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(-8 + s V_id3_crc_calculate__tmp) <= z)%Q
   | 40 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 41 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 42 => ((1 # 1) + s V_id3_crc_calculate_z
            + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | 43 => (s V_id3_crc_calculate_z
            + (1 # 8) * max0(s V_id3_crc_calculate__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_id3_crc_calculate =>
    [mkPA Q (fun n z s => ai_id3_crc_calculate n s /\ annot0_id3_crc_calculate n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_id3_crc_calculate (proc_start P_id3_crc_calculate) s1 (proc_end P_id3_crc_calculate) s2 ->
    (s2 V_id3_crc_calculate_z <= (1 # 8) * max0(s1 V_id3_crc_calculate_length))%Q.
Proof.
  prove_bound ipa admissible_ipa P_id3_crc_calculate.
Qed.
