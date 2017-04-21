Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_cache_mult.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_cache_mult_z := 1%positive.
Notation V_cie_cache_mult_i := 2%positive.
Notation V_cie_cache_mult_pcache := 3%positive.
Notation V_cie_cache_mult_pcf := 4%positive.
Notation V_cie_cache_mult_pvec := 5%positive.
Definition Pedges_cie_cache_mult: list (edge proc) :=
  (EA 1 (AAssign V_cie_cache_mult_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cie_cache_mult_i (Some (ENum (512)))) 3)::(EA 3 ANone 4)::(EA 4 (AAssign
  V_cie_cache_mult_i (Some (EAdd (EVar V_cie_cache_mult_i)
  (ENum (-1))))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EAdd (EVar V_cie_cache_mult_i) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EAdd (EVar V_cie_cache_mult_i) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 11 ANone 12)::(EA 12 (AAssign V_cie_cache_mult_z
  (Some (EAdd (ENum (1)) (EVar V_cie_cache_mult_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_cache_mult => Pedges_cie_cache_mult
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_cache_mult => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_cache_mult (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_cache_mult_z <= 0 /\ -1 * s V_cie_cache_mult_z <= 0)%Z
   | 3 => (-1 * s V_cie_cache_mult_z <= 0 /\ 1 * s V_cie_cache_mult_z <= 0 /\ 1 * s V_cie_cache_mult_i + -512 <= 0 /\ -1 * s V_cie_cache_mult_i + 512 <= 0)%Z
   | 4 => (1 * s V_cie_cache_mult_i + -512 <= 0 /\ -1 * s V_cie_cache_mult_z <= 0 /\ -1 * s V_cie_cache_mult_i + 1 <= 0)%Z
   | 5 => (-1 * s V_cie_cache_mult_z <= 0 /\ 1 * s V_cie_cache_mult_i + -511 <= 0 /\ -1 * s V_cie_cache_mult_i <= 0)%Z
   | 6 => (-1 * s V_cie_cache_mult_i <= 0 /\ 1 * s V_cie_cache_mult_i + -511 <= 0 /\ -1 * s V_cie_cache_mult_z <= 0)%Z
   | 7 => (-1 * s V_cie_cache_mult_z <= 0 /\ -1 * s V_cie_cache_mult_i <= 0 /\ 1 * s V_cie_cache_mult_i <= 0)%Z
   | 8 => (1 * s V_cie_cache_mult_i <= 0 /\ -1 * s V_cie_cache_mult_i <= 0 /\ -1 * s V_cie_cache_mult_z <= 0)%Z
   | 9 => (-1 * s V_cie_cache_mult_z <= 0 /\ 1 * s V_cie_cache_mult_i + -511 <= 0 /\ -1 * s V_cie_cache_mult_i + 1 <= 0)%Z
   | 10 => (-1 * s V_cie_cache_mult_i + 1 <= 0 /\ 1 * s V_cie_cache_mult_i + -511 <= 0 /\ -1 * s V_cie_cache_mult_z <= 0)%Z
   | 11 => (-1 * s V_cie_cache_mult_z <= 0 /\ 1 * s V_cie_cache_mult_i + -511 <= 0 /\ -1 * s V_cie_cache_mult_i + 1 <= 0)%Z
   | 12 => (-1 * s V_cie_cache_mult_i + 1 <= 0 /\ 1 * s V_cie_cache_mult_i + -511 <= 0 /\ -1 * s V_cie_cache_mult_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_cache_mult (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((511 # 1) <= z)%Q
   | 2 => ((511 # 1) + s V_cie_cache_mult_z <= z)%Q
   | 3 => (-(1 # 1) + s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 4 => (-(1 # 1) + s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 5 => (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 6 => (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_cie_cache_mult_i) (-1
                                                                    + 
                                                                    s V_cie_cache_mult_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_cache_mult_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cie_cache_mult_i) (0))) (F_max0_ge_0 (s V_cie_cache_mult_i))]
     (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 8 => (s V_cie_cache_mult_z <= z)%Q
   | 9 => (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 10 => (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 11 => (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | 12 => (s V_cie_cache_mult_i + s V_cie_cache_mult_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_cache_mult =>
    [mkPA Q (fun n z s => ai_cie_cache_mult n s /\ annot0_cie_cache_mult n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_cache_mult (proc_start P_cie_cache_mult) s1 (proc_end P_cie_cache_mult) s2 ->
    (s2 V_cie_cache_mult_z <= (511 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_cache_mult.
Qed.
