Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bi_reverse.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bi_reverse_z := 1%positive.
Notation V_bi_reverse__tmp := 2%positive.
Notation V_bi_reverse__tmp1 := 3%positive.
Notation V_bi_reverse_res := 4%positive.
Notation V_bi_reverse_code := 5%positive.
Notation V_bi_reverse_len := 6%positive.
Definition Pedges_bi_reverse: list (edge proc) :=
  (EA 1 (AAssign V_bi_reverse_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_bi_reverse__tmp1 (Some (EVar V_bi_reverse_code))) 3)::(EA 3 (AAssign
  V_bi_reverse__tmp (Some (EVar V_bi_reverse_len))) 4)::(EA 4 (AAssign
  V_bi_reverse_res (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 (AAssign
  V_bi_reverse_res None) 7)::(EA 7 (AAssign V_bi_reverse__tmp1 None) 8)::
  (EA 8 (AAssign V_bi_reverse_res None) 9)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_bi_reverse__tmp (Some (EAdd (EVar V_bi_reverse__tmp) (ENum (-1))))) 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EAdd (EVar V_bi_reverse__tmp) (ENum (-1))) s) >
  (eval (ENum (0)) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EAdd (EVar V_bi_reverse__tmp) (ENum (-1))) s) <=
  (eval (ENum (0)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 15 AWeaken 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_bi_reverse_z (Some (EAdd (ENum (1))
  (EVar V_bi_reverse_z)))) 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bi_reverse => Pedges_bi_reverse
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bi_reverse => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_bi_reverse (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bi_reverse_z <= 0 /\ -1 * s V_bi_reverse_z <= 0)%Z
   | 3 => (-1 * s V_bi_reverse_z <= 0 /\ 1 * s V_bi_reverse_z <= 0)%Z
   | 4 => (1 * s V_bi_reverse_z <= 0 /\ -1 * s V_bi_reverse_z <= 0)%Z
   | 5 => (-1 * s V_bi_reverse_z <= 0 /\ 1 * s V_bi_reverse_z <= 0 /\ 1 * s V_bi_reverse_res <= 0 /\ -1 * s V_bi_reverse_res <= 0)%Z
   | 6 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 7 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 8 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 9 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 10 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 11 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 12 => (-1 * s V_bi_reverse_z <= 0)%Z
   | 13 => (-1 * s V_bi_reverse_z <= 0 /\ 1 * s V_bi_reverse__tmp + -1 <= 0)%Z
   | 14 => (1 * s V_bi_reverse__tmp + -1 <= 0 /\ -1 * s V_bi_reverse_z <= 0)%Z
   | 15 => (-1 * s V_bi_reverse_z <= 0 /\ -1 * s V_bi_reverse__tmp + 2 <= 0)%Z
   | 16 => (-1 * s V_bi_reverse__tmp + 2 <= 0 /\ -1 * s V_bi_reverse_z <= 0)%Z
   | 17 => (-1 * s V_bi_reverse_z <= 0 /\ -1 * s V_bi_reverse__tmp + 2 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bi_reverse (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-2 + s V_bi_reverse_len) <= z)%Q
   | 2 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse_len) <= z)%Q
   | 3 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse_len) <= z)%Q
   | 4 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 5 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 6 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 7 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 8 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 9 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 10 => (s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 11 => (s V_bi_reverse_z + max0(-1 + s V_bi_reverse__tmp) <= z)%Q
   | 12 => (s V_bi_reverse_z + max0(-1 + s V_bi_reverse__tmp) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_bi_reverse__tmp) (-2
                                                                    + s V_bi_reverse__tmp));
      (*-1 0*) F_max0_ge_0 (-2 + s V_bi_reverse__tmp)]
     (s V_bi_reverse_z + max0(-1 + s V_bi_reverse__tmp) <= z)%Q
   | 14 => (s V_bi_reverse_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-1 + s V_bi_reverse__tmp) (1)]
     (s V_bi_reverse_z + max0(-1 + s V_bi_reverse__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_bi_reverse_z + max0(-2 + s V_bi_reverse__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bi_reverse =>
    [mkPA Q (fun n z s => ai_bi_reverse n s /\ annot0_bi_reverse n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bi_reverse (proc_start P_bi_reverse) s1 (proc_end P_bi_reverse) s2 ->
    (s2 V_bi_reverse_z <= max0(-2 + s1 V_bi_reverse_len))%Q.
Proof.
  prove_bound ipa admissible_ipa P_bi_reverse.
Qed.
