Require Import pasta.Pasta.

Inductive proc: Type :=
  P_flip3x4.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_flip3x4_z := 1%positive.
Notation V_flip3x4__tmp := 2%positive.
Notation V_flip3x4__tmp1 := 3%positive.
Notation V_flip3x4_b1 := 4%positive.
Notation V_flip3x4_b2 := 5%positive.
Notation V_flip3x4_b3 := 6%positive.
Notation V_flip3x4_n := 7%positive.
Notation V_flip3x4_buffer := 8%positive.
Notation V_flip3x4_nbytes := 9%positive.
Notation V_flip3x4_offset := 10%positive.
Notation V_flip3x4_planes := 11%positive.
Definition Pedges_flip3x4: list (edge proc) :=
  (EA 1 (AAssign V_flip3x4_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_flip3x4_n) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_flip3x4__tmp1
  (Some (EVar V_flip3x4_offset))) 5)::(EA 5 (AAssign V_flip3x4__tmp
  (Some (EVar V_flip3x4_nbytes))) 6)::(EA 6 (AAssign V_flip3x4_n
  (Some (EVar V_flip3x4__tmp))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_flip3x4_n) s) > (eval (ENum (0))
  s))%Z)) 12)::(EA 9 (AGuard (fun s => ((eval (EVar V_flip3x4_n) s) <=
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_flip3x4_b1 None) 14)::(EA 14 (AAssign V_flip3x4_b2
  None) 15)::(EA 15 (AAssign V_flip3x4_b3 None) 16)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_flip3x4_n (Some (EAdd (EVar V_flip3x4_n)
  (ENum (-1))))) 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_flip3x4_z (Some (EAdd (ENum (1)) (EVar V_flip3x4_z)))) 21)::
  (EA 21 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_flip3x4 => Pedges_flip3x4
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_flip3x4 => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_flip3x4 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 3 => (-1 * s V_flip3x4_z <= 0 /\ 1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n <= 0)%Z
   | 4 => (-1 * s V_flip3x4_n <= 0 /\ 1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 5 => (-1 * s V_flip3x4_z <= 0 /\ 1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n <= 0)%Z
   | 6 => (-1 * s V_flip3x4_n <= 0 /\ 1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 7 => (-1 * s V_flip3x4_z <= 0 /\ 1 * s V_flip3x4_z <= 0)%Z
   | 8 => (1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 9 => (-1 * s V_flip3x4_z <= 0)%Z
   | 10 => (-1 * s V_flip3x4_z <= 0 /\ 1 * s V_flip3x4_n <= 0)%Z
   | 11 => (1 * s V_flip3x4_n <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 12 => (-1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n + 1 <= 0)%Z
   | 13 => (-1 * s V_flip3x4_n + 1 <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 14 => (-1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n + 1 <= 0)%Z
   | 15 => (-1 * s V_flip3x4_n + 1 <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 16 => (-1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n + 1 <= 0)%Z
   | 17 => (-1 * s V_flip3x4_n + 1 <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 18 => (-1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n <= 0)%Z
   | 19 => (-1 * s V_flip3x4_n <= 0 /\ -1 * s V_flip3x4_z <= 0)%Z
   | 20 => (-1 * s V_flip3x4_z <= 0 /\ -1 * s V_flip3x4_n <= 0)%Z
   | 21 => (-1 * s V_flip3x4_n <= 0 /\ -1 * s V_flip3x4_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_flip3x4 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_flip3x4_nbytes) <= z)%Q
   | 2 => (s V_flip3x4_z + max0(s V_flip3x4_nbytes) <= z)%Q
   | 3 => (s V_flip3x4_z + max0(s V_flip3x4_nbytes) <= z)%Q
   | 4 => (s V_flip3x4_z + max0(s V_flip3x4_nbytes) <= z)%Q
   | 5 => (s V_flip3x4_z + max0(s V_flip3x4_nbytes) <= z)%Q
   | 6 => (s V_flip3x4_z + max0(s V_flip3x4__tmp) <= z)%Q
   | 7 => (s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 8 => (s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 9 => (s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_flip3x4_n) (-1
                                                             + s V_flip3x4_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_flip3x4_n)]
     (s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 11 => (s V_flip3x4_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_flip3x4_n) (1)]
     (s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 13 => ((1 # 1) + s V_flip3x4_z + max0(-1 + s V_flip3x4_n) <= z)%Q
   | 14 => ((1 # 1) + s V_flip3x4_z + max0(-1 + s V_flip3x4_n) <= z)%Q
   | 15 => ((1 # 1) + s V_flip3x4_z + max0(-1 + s V_flip3x4_n) <= z)%Q
   | 16 => ((1 # 1) + s V_flip3x4_z + max0(-1 + s V_flip3x4_n) <= z)%Q
   | 17 => ((1 # 1) + s V_flip3x4_z + max0(-1 + s V_flip3x4_n) <= z)%Q
   | 18 => ((1 # 1) + s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 19 => ((1 # 1) + s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 20 => ((1 # 1) + s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | 21 => (s V_flip3x4_z + max0(s V_flip3x4_n) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_flip3x4 =>
    [mkPA Q (fun n z s => ai_flip3x4 n s /\ annot0_flip3x4 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_flip3x4 (proc_start P_flip3x4) s1 (proc_end P_flip3x4) s2 ->
    (s2 V_flip3x4_z <= max0(s1 V_flip3x4_nbytes))%Q.
Proof.
  prove_bound ipa admissible_ipa P_flip3x4.
Qed.
