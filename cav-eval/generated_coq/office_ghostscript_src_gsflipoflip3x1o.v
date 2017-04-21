Require Import pasta.Pasta.

Inductive proc: Type :=
  P_flip3x1.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_flip3x1_z := 1%positive.
Notation V_flip3x1__tmp := 2%positive.
Notation V_flip3x1__tmp1 := 3%positive.
Notation V_flip3x1_b24 := 4%positive.
Notation V_flip3x1_n := 5%positive.
Notation V_flip3x1_buffer := 6%positive.
Notation V_flip3x1_nbytes := 7%positive.
Notation V_flip3x1_offset := 8%positive.
Notation V_flip3x1_planes := 9%positive.
Definition Pedges_flip3x1: list (edge proc) :=
  (EA 1 (AAssign V_flip3x1_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_flip3x1_n) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_flip3x1__tmp1
  (Some (EVar V_flip3x1_offset))) 5)::(EA 5 (AAssign V_flip3x1__tmp
  (Some (EVar V_flip3x1_nbytes))) 6)::(EA 6 (AAssign V_flip3x1_n
  (Some (EVar V_flip3x1__tmp))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_flip3x1_n) s) > (eval (ENum (0))
  s))%Z)) 12)::(EA 9 (AGuard (fun s => ((eval (EVar V_flip3x1_n) s) <=
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_flip3x1_b24 None) 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_flip3x1_n (Some (EAdd (EVar V_flip3x1_n) (ENum (-1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_flip3x1_z
  (Some (EAdd (ENum (1)) (EVar V_flip3x1_z)))) 19)::(EA 19 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_flip3x1 => Pedges_flip3x1
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_flip3x1 => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_flip3x1 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 3 => (-1 * s V_flip3x1_z <= 0 /\ 1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_n <= 0)%Z
   | 4 => (-1 * s V_flip3x1_n <= 0 /\ 1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 5 => (-1 * s V_flip3x1_z <= 0 /\ 1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_n <= 0)%Z
   | 6 => (-1 * s V_flip3x1_n <= 0 /\ 1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 7 => (-1 * s V_flip3x1_z <= 0 /\ 1 * s V_flip3x1_z <= 0)%Z
   | 8 => (1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 9 => (-1 * s V_flip3x1_z <= 0)%Z
   | 10 => (-1 * s V_flip3x1_z <= 0 /\ 1 * s V_flip3x1_n <= 0)%Z
   | 11 => (1 * s V_flip3x1_n <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 12 => (-1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_n + 1 <= 0)%Z
   | 13 => (-1 * s V_flip3x1_n + 1 <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 14 => (-1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_n + 1 <= 0)%Z
   | 15 => (-1 * s V_flip3x1_n + 1 <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 16 => (-1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_n <= 0)%Z
   | 17 => (-1 * s V_flip3x1_n <= 0 /\ -1 * s V_flip3x1_z <= 0)%Z
   | 18 => (-1 * s V_flip3x1_z <= 0 /\ -1 * s V_flip3x1_n <= 0)%Z
   | 19 => (-1 * s V_flip3x1_n <= 0 /\ -1 * s V_flip3x1_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_flip3x1 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_flip3x1_nbytes) <= z)%Q
   | 2 => (s V_flip3x1_z + max0(s V_flip3x1_nbytes) <= z)%Q
   | 3 => (s V_flip3x1_z + max0(s V_flip3x1_nbytes) <= z)%Q
   | 4 => (s V_flip3x1_z + max0(s V_flip3x1_nbytes) <= z)%Q
   | 5 => (s V_flip3x1_z + max0(s V_flip3x1_nbytes) <= z)%Q
   | 6 => (s V_flip3x1_z + max0(s V_flip3x1__tmp) <= z)%Q
   | 7 => (s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 8 => (s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 9 => (s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_flip3x1_n) (-1
                                                             + s V_flip3x1_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_flip3x1_n)]
     (s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 11 => (s V_flip3x1_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_flip3x1_n) (1)]
     (s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 13 => ((1 # 1) + s V_flip3x1_z + max0(-1 + s V_flip3x1_n) <= z)%Q
   | 14 => ((1 # 1) + s V_flip3x1_z + max0(-1 + s V_flip3x1_n) <= z)%Q
   | 15 => ((1 # 1) + s V_flip3x1_z + max0(-1 + s V_flip3x1_n) <= z)%Q
   | 16 => ((1 # 1) + s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 17 => ((1 # 1) + s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 18 => ((1 # 1) + s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | 19 => (s V_flip3x1_z + max0(s V_flip3x1_n) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_flip3x1 =>
    [mkPA Q (fun n z s => ai_flip3x1 n s /\ annot0_flip3x1 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_flip3x1 (proc_start P_flip3x1) s1 (proc_end P_flip3x1) s2 ->
    (s2 V_flip3x1_z <= max0(s1 V_flip3x1_nbytes))%Q.
Proof.
  prove_bound ipa admissible_ipa P_flip3x1.
Qed.
