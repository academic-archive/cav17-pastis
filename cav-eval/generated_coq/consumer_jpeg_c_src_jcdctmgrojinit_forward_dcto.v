Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jinit_forward_dct.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jinit_forward_dct_z := 1%positive.
Notation V_jinit_forward_dct_i := 2%positive.
Notation V_jinit_forward_dct_cinfo := 3%positive.
Definition Pedges_jinit_forward_dct: list (edge proc) :=
  (EA 1 (AAssign V_jinit_forward_dct_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 7)::(EA 3 ANone 6)::(EA 3 ANone 5)::
  (EA 3 ANone 4)::(EA 4 ANone 8)::(EA 5 ANone 8)::(EA 6 ANone 8)::
  (EA 7 ANone 8)::(EA 8 (AAssign V_jinit_forward_dct_i
  (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jinit_forward_dct_i) s) < (eval (ENum (4))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_jinit_forward_dct_i)
  s) >= (eval (ENum (4)) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_jinit_forward_dct_i
  (Some (EAdd (EVar V_jinit_forward_dct_i) (ENum (1))))) 17)::
  (EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_jinit_forward_dct_z
  (Some (EAdd (ENum (1)) (EVar V_jinit_forward_dct_z)))) 20)::
  (EA 20 AWeaken 11)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jinit_forward_dct => Pedges_jinit_forward_dct
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jinit_forward_dct => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_jinit_forward_dct (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0)%Z
   | 3 => (-1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_z <= 0)%Z
   | 4 => (1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0)%Z
   | 5 => (1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0)%Z
   | 6 => (1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0)%Z
   | 7 => (1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0)%Z
   | 8 => (-1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_z <= 0)%Z
   | 9 => (1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_i <= 0 /\ -1 * s V_jinit_forward_dct_i <= 0)%Z
   | 10 => (-1 * s V_jinit_forward_dct_i <= 0 /\ 1 * s V_jinit_forward_dct_i <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_z <= 0)%Z
   | 11 => (-1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_i <= 0 /\ 1 * s V_jinit_forward_dct_i + -4 <= 0)%Z
   | 12 => (1 * s V_jinit_forward_dct_i + -4 <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_i + 4 <= 0)%Z
   | 13 => (-1 * s V_jinit_forward_dct_i + 4 <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_i + -4 <= 0)%Z
   | 14 => (-1 * s V_jinit_forward_dct_i <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_i + -3 <= 0)%Z
   | 15 => (1 * s V_jinit_forward_dct_i + -3 <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_i <= 0)%Z
   | 16 => (-1 * s V_jinit_forward_dct_i <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0 /\ 1 * s V_jinit_forward_dct_i + -3 <= 0)%Z
   | 17 => (-1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_i + 1 <= 0 /\ 1 * s V_jinit_forward_dct_i + -4 <= 0)%Z
   | 18 => (1 * s V_jinit_forward_dct_i + -4 <= 0 /\ -1 * s V_jinit_forward_dct_i + 1 <= 0 /\ -1 * s V_jinit_forward_dct_z <= 0)%Z
   | 19 => (-1 * s V_jinit_forward_dct_z <= 0 /\ -1 * s V_jinit_forward_dct_i + 1 <= 0 /\ 1 * s V_jinit_forward_dct_i + -4 <= 0)%Z
   | 20 => (1 * s V_jinit_forward_dct_i + -4 <= 0 /\ -1 * s V_jinit_forward_dct_i + 1 <= 0 /\ -1 * s V_jinit_forward_dct_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jinit_forward_dct (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 3 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 4 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 5 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 6 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 7 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 8 => ((4 # 1) + s V_jinit_forward_dct_z <= z)%Q
   | 9 => (s V_jinit_forward_dct_z + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 10 => (s V_jinit_forward_dct_z + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 11 => (s V_jinit_forward_dct_z + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_jinit_forward_dct_i) (3
                                                                    - s V_jinit_forward_dct_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_jinit_forward_dct_i)]
     (s V_jinit_forward_dct_z + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 13 => (s V_jinit_forward_dct_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_jinit_forward_dct_i) (1)]
     (s V_jinit_forward_dct_z + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 15 => ((1 # 1) + s V_jinit_forward_dct_z
            + max0(3 - s V_jinit_forward_dct_i) <= z)%Q
   | 16 => ((1 # 1) + s V_jinit_forward_dct_z
            + max0(3 - s V_jinit_forward_dct_i) <= z)%Q
   | 17 => ((1 # 1) + s V_jinit_forward_dct_z
            + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 18 => ((1 # 1) + s V_jinit_forward_dct_z
            + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 19 => ((1 # 1) + s V_jinit_forward_dct_z
            + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | 20 => (s V_jinit_forward_dct_z + max0(4 - s V_jinit_forward_dct_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jinit_forward_dct =>
    [mkPA Q (fun n z s => ai_jinit_forward_dct n s /\ annot0_jinit_forward_dct n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jinit_forward_dct (proc_start P_jinit_forward_dct) s1 (proc_end P_jinit_forward_dct) s2 ->
    (s2 V_jinit_forward_dct_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jinit_forward_dct.
Qed.
