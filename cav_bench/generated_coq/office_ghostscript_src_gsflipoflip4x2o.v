Require Import pasta.Pasta.

Inductive proc: Type :=
  P_flip4x2.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_flip4x2_z := 1%positive.
Notation V_flip4x2__tmp := 2%positive.
Notation V_flip4x2__tmp1 := 3%positive.
Notation V_flip4x2_b1 := 4%positive.
Notation V_flip4x2_b2 := 5%positive.
Notation V_flip4x2_b3 := 6%positive.
Notation V_flip4x2_b4 := 7%positive.
Notation V_flip4x2_n := 8%positive.
Notation V_flip4x2_temp := 9%positive.
Notation V_flip4x2_buffer := 10%positive.
Notation V_flip4x2_nbytes := 11%positive.
Notation V_flip4x2_offset := 12%positive.
Notation V_flip4x2_planes := 13%positive.
Definition Pedges_flip4x2: list (edge proc) :=
  (EA 1 (AAssign V_flip4x2_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_flip4x2_n) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_flip4x2__tmp1
  (Some (EVar V_flip4x2_offset))) 5)::(EA 5 (AAssign V_flip4x2__tmp
  (Some (EVar V_flip4x2_nbytes))) 6)::(EA 6 (AAssign V_flip4x2_n
  (Some (EVar V_flip4x2__tmp))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_flip4x2_n) s) > (eval (ENum (0))
  s))%Z)) 12)::(EA 9 (AGuard (fun s => ((eval (EVar V_flip4x2_n) s) <=
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_flip4x2_b1 None) 14)::(EA 14 (AAssign V_flip4x2_b2
  None) 15)::(EA 15 (AAssign V_flip4x2_b3 None) 16)::(EA 16 (AAssign
  V_flip4x2_b4 None) 17)::(EA 17 (AAssign V_flip4x2_temp None) 18)::
  (EA 18 (AAssign V_flip4x2_b1 None) 19)::(EA 19 (AAssign V_flip4x2_b3
  None) 20)::(EA 20 (AAssign V_flip4x2_temp None) 21)::(EA 21 (AAssign
  V_flip4x2_b2 None) 22)::(EA 22 (AAssign V_flip4x2_b4 None) 23)::
  (EA 23 (AAssign V_flip4x2_temp None) 24)::(EA 24 (AAssign V_flip4x2_b1
  None) 25)::(EA 25 (AAssign V_flip4x2_b2 None) 26)::(EA 26 (AAssign
  V_flip4x2_temp None) 27)::(EA 27 (AAssign V_flip4x2_b3 None) 28)::
  (EA 28 (AAssign V_flip4x2_b4 None) 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_flip4x2_n (Some (EAdd (EVar V_flip4x2_n) (ENum (-1))))) 31)::
  (EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 (AAssign V_flip4x2_z
  (Some (EAdd (ENum (1)) (EVar V_flip4x2_z)))) 34)::(EA 34 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_flip4x2 => Pedges_flip4x2
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_flip4x2 => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_flip4x2 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 3 => (-1 * s V_flip4x2_z <= 0 /\ 1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n <= 0)%Z
   | 4 => (-1 * s V_flip4x2_n <= 0 /\ 1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 5 => (-1 * s V_flip4x2_z <= 0 /\ 1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n <= 0)%Z
   | 6 => (-1 * s V_flip4x2_n <= 0 /\ 1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 7 => (-1 * s V_flip4x2_z <= 0 /\ 1 * s V_flip4x2_z <= 0)%Z
   | 8 => (1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 9 => (-1 * s V_flip4x2_z <= 0)%Z
   | 10 => (-1 * s V_flip4x2_z <= 0 /\ 1 * s V_flip4x2_n <= 0)%Z
   | 11 => (1 * s V_flip4x2_n <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 12 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 13 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 14 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 15 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 16 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 17 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 18 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 19 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 20 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 21 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 22 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 23 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 24 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 25 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 26 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 27 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 28 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 29 => (-1 * s V_flip4x2_n + 1 <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 30 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n + 1 <= 0)%Z
   | 31 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n <= 0)%Z
   | 32 => (-1 * s V_flip4x2_n <= 0 /\ -1 * s V_flip4x2_z <= 0)%Z
   | 33 => (-1 * s V_flip4x2_z <= 0 /\ -1 * s V_flip4x2_n <= 0)%Z
   | 34 => (-1 * s V_flip4x2_n <= 0 /\ -1 * s V_flip4x2_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_flip4x2 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_flip4x2_nbytes) <= z)%Q
   | 2 => (s V_flip4x2_z + max0(s V_flip4x2_nbytes) <= z)%Q
   | 3 => (s V_flip4x2_z + max0(s V_flip4x2_nbytes) <= z)%Q
   | 4 => (s V_flip4x2_z + max0(s V_flip4x2_nbytes) <= z)%Q
   | 5 => (s V_flip4x2_z + max0(s V_flip4x2_nbytes) <= z)%Q
   | 6 => (s V_flip4x2_z + max0(s V_flip4x2__tmp) <= z)%Q
   | 7 => (s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 8 => (s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 9 => (s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_flip4x2_n) (-1
                                                             + s V_flip4x2_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_flip4x2_n)]
     (s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 11 => (s V_flip4x2_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_flip4x2_n) (1)]
     (s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 13 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 14 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 15 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 16 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 17 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 18 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 19 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 20 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 21 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 22 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 23 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 24 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 25 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 26 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 27 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 28 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 29 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 30 => ((1 # 1) + s V_flip4x2_z + max0(-1 + s V_flip4x2_n) <= z)%Q
   | 31 => ((1 # 1) + s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 32 => ((1 # 1) + s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 33 => ((1 # 1) + s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | 34 => (s V_flip4x2_z + max0(s V_flip4x2_n) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_flip4x2 =>
    [mkPA Q (fun n z s => ai_flip4x2 n s /\ annot0_flip4x2 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_flip4x2 (proc_start P_flip4x2) s1 (proc_end P_flip4x2) s2 ->
    (s2 V_flip4x2_z <= max0(s1 V_flip4x2_nbytes))%Q.
Proof.
  prove_bound ipa admissible_ipa P_flip4x2.
Qed.
