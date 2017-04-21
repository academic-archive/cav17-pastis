Require Import pasta.Pasta.

Inductive proc: Type :=
  P_build_bl_tree.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_build_bl_tree_z := 1%positive.
Notation V_build_bl_tree_max_blindex := 2%positive.
Notation V_build_bl_tree_opt_len := 3%positive.
Definition Pedges_build_bl_tree: list (edge proc) :=
  (EA 1 (AAssign V_build_bl_tree_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_build_bl_tree_max_blindex (Some (ENum (18)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_build_bl_tree_max_blindex) s) >= (eval (ENum (3))
  s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_build_bl_tree_max_blindex) s) < (eval (ENum (3))
  s))%Z)) 6)::(EA 6 AWeaken 16)::(EA 7 AWeaken 8)::(EA 8 ANone 15)::
  (EA 8 ANone 9)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_build_bl_tree_max_blindex (Some (EAdd (EVar V_build_bl_tree_max_blindex)
  (ENum (-1))))) 11)::(EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_build_bl_tree_z (Some (EAdd (ENum (1)) (EVar V_build_bl_tree_z)))) 14)::
  (EA 14 AWeaken 5)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_build_bl_tree_opt_len (Some (EAdd (EVar V_build_bl_tree_opt_len)
  (EAdd (EAdd (EAdd (EMul (ENum (3)) (EAdd (EVar V_build_bl_tree_max_blindex)
  (ENum (1)))) (ENum (5))) (ENum (5))) (ENum (4)))))) 17)::
  (EA 17 AWeaken 18)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_build_bl_tree => Pedges_build_bl_tree
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_build_bl_tree => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_build_bl_tree (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_z <= 0)%Z
   | 3 => (-1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 18 <= 0)%Z
   | 4 => (-1 * s V_build_bl_tree_max_blindex + 18 <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ 1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_z <= 0)%Z
   | 5 => (-1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 2 <= 0)%Z
   | 6 => (-1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -2 <= 0)%Z
   | 7 => (1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 3 <= 0)%Z
   | 8 => (-1 * s V_build_bl_tree_max_blindex + 3 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0)%Z
   | 9 => (1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 3 <= 0)%Z
   | 10 => (-1 * s V_build_bl_tree_max_blindex + 3 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0)%Z
   | 11 => (-1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -17 <= 0)%Z
   | 12 => (1 * s V_build_bl_tree_max_blindex + -17 <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ -1 * s V_build_bl_tree_z <= 0)%Z
   | 13 => (-1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -17 <= 0)%Z
   | 14 => (1 * s V_build_bl_tree_max_blindex + -17 <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ -1 * s V_build_bl_tree_z + 1 <= 0)%Z
   | 15 => (1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 3 <= 0)%Z
   | 16 => (-1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0)%Z
   | 17 => (1 * s V_build_bl_tree_max_blindex + -18 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ -1 * s V_build_bl_tree_max_blindex + 2 <= 0)%Z
   | 18 => (-1 * s V_build_bl_tree_max_blindex + 2 <= 0 /\ -1 * s V_build_bl_tree_z <= 0 /\ 1 * s V_build_bl_tree_max_blindex + -18 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_build_bl_tree (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_build_bl_tree_z <= z)%Q
   | 3 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 4 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 5 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 6 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 7 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 8 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 9 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 10 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 11 => (-(1 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 12 => (-(1 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 13 => (-(1 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 14 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 15 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 16 => (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-2
                                             + s V_build_bl_tree_max_blindex) (-3
                                                                    + s V_build_bl_tree_max_blindex));
      (*-1 0*) F_max0_ge_0 (-3 + s V_build_bl_tree_max_blindex);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                               + s V_build_bl_tree_max_blindex) (0))) (F_max0_ge_0 (-2
                                                                    + s V_build_bl_tree_max_blindex))]
     (-(2 # 1) + s V_build_bl_tree_max_blindex + s V_build_bl_tree_z <= z)%Q
   | 18 => (s V_build_bl_tree_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_build_bl_tree =>
    [mkPA Q (fun n z s => ai_build_bl_tree n s /\ annot0_build_bl_tree n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_build_bl_tree (proc_start P_build_bl_tree) s1 (proc_end P_build_bl_tree) s2 ->
    (s2 V_build_bl_tree_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_build_bl_tree.
Qed.
