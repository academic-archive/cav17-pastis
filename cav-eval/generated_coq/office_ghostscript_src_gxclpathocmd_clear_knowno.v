Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_clear_known.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_clear_known_z := 1%positive.
Notation V_cmd_clear_known__tmp := 2%positive.
Notation V_cmd_clear_known_cldev_dref_off784 := 3%positive.
Notation V_cmd_clear_known_i := 4%positive.
Notation V_cmd_clear_known_unknown := 5%positive.
Notation V_cmd_clear_known_cldev := 6%positive.
Notation V_cmd_clear_known_known := 7%positive.
Definition Pedges_cmd_clear_known: list (edge proc) :=
  (EA 1 (AAssign V_cmd_clear_known_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cmd_clear_known__tmp (Some (EVar V_cmd_clear_known_known))) 3)::
  (EA 3 (AAssign V_cmd_clear_known_unknown None) 4)::(EA 4 (AAssign
  V_cmd_clear_known_i (Some (EVar V_cmd_clear_known_cldev_dref_off784))) 5)::
  (EA 5 ANone 6)::(EA 6 (AAssign V_cmd_clear_known_i
  (Some (EAdd (EVar V_cmd_clear_known_i) (ENum (-1))))) 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EAdd (EVar V_cmd_clear_known_i) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EAdd (EVar V_cmd_clear_known_i) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::
  (EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_cmd_clear_known_z (Some (EAdd (ENum (1))
  (EVar V_cmd_clear_known_z)))) 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_clear_known => Pedges_cmd_clear_known
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_clear_known => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_clear_known (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_clear_known_z <= 0 /\ -1 * s V_cmd_clear_known_z <= 0)%Z
   | 3 => (-1 * s V_cmd_clear_known_z <= 0 /\ 1 * s V_cmd_clear_known_z <= 0)%Z
   | 4 => (1 * s V_cmd_clear_known_z <= 0 /\ -1 * s V_cmd_clear_known_z <= 0)%Z
   | 5 => (-1 * s V_cmd_clear_known_z <= 0 /\ 1 * s V_cmd_clear_known_z <= 0)%Z
   | 6 => (-1 * s V_cmd_clear_known_z <= 0)%Z
   | 7 => (-1 * s V_cmd_clear_known_z <= 0)%Z
   | 8 => (-1 * s V_cmd_clear_known_z <= 0)%Z
   | 9 => (-1 * s V_cmd_clear_known_z <= 0 /\ 1 * s V_cmd_clear_known_i <= 0)%Z
   | 10 => (1 * s V_cmd_clear_known_i <= 0 /\ -1 * s V_cmd_clear_known_z <= 0)%Z
   | 11 => (-1 * s V_cmd_clear_known_z <= 0 /\ -1 * s V_cmd_clear_known_i + 1 <= 0)%Z
   | 12 => (-1 * s V_cmd_clear_known_i + 1 <= 0 /\ -1 * s V_cmd_clear_known_z <= 0)%Z
   | 13 => (-1 * s V_cmd_clear_known_z <= 0 /\ -1 * s V_cmd_clear_known_i + 1 <= 0)%Z
   | 14 => (-1 * s V_cmd_clear_known_i + 1 <= 0 /\ -1 * s V_cmd_clear_known_z <= 0)%Z
   | 15 => (-1 * s V_cmd_clear_known_z <= 0 /\ -1 * s V_cmd_clear_known_i + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_clear_known (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_cmd_clear_known_cldev_dref_off784) <= z)%Q
   | 2 => (s V_cmd_clear_known_z
           + max0(-1 + s V_cmd_clear_known_cldev_dref_off784) <= z)%Q
   | 3 => (s V_cmd_clear_known_z
           + max0(-1 + s V_cmd_clear_known_cldev_dref_off784) <= z)%Q
   | 4 => (s V_cmd_clear_known_z
           + max0(-1 + s V_cmd_clear_known_cldev_dref_off784) <= z)%Q
   | 5 => (s V_cmd_clear_known_z + max0(-1 + s V_cmd_clear_known_i) <= z)%Q
   | 6 => (s V_cmd_clear_known_z + max0(-1 + s V_cmd_clear_known_i) <= z)%Q
   | 7 => (s V_cmd_clear_known_z + max0(s V_cmd_clear_known_i) <= z)%Q
   | 8 => (s V_cmd_clear_known_z + max0(s V_cmd_clear_known_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_cmd_clear_known_i) (-1
                                                                    + 
                                                                    s V_cmd_clear_known_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cmd_clear_known_i)]
     (s V_cmd_clear_known_z + max0(s V_cmd_clear_known_i) <= z)%Q
   | 10 => (s V_cmd_clear_known_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_cmd_clear_known_i) (1)]
     (s V_cmd_clear_known_z + max0(s V_cmd_clear_known_i) <= z)%Q
   | 12 => ((1 # 1) + s V_cmd_clear_known_z
            + max0(-1 + s V_cmd_clear_known_i) <= z)%Q
   | 13 => ((1 # 1) + s V_cmd_clear_known_z
            + max0(-1 + s V_cmd_clear_known_i) <= z)%Q
   | 14 => ((1 # 1) + s V_cmd_clear_known_z
            + max0(-1 + s V_cmd_clear_known_i) <= z)%Q
   | 15 => ((1 # 1) + s V_cmd_clear_known_z
            + max0(-1 + s V_cmd_clear_known_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_clear_known =>
    [mkPA Q (fun n z s => ai_cmd_clear_known n s /\ annot0_cmd_clear_known n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_clear_known (proc_start P_cmd_clear_known) s1 (proc_end P_cmd_clear_known) s2 ->
    (s2 V_cmd_clear_known_z <= max0(-1
                                    + s1 V_cmd_clear_known_cldev_dref_off784))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_clear_known.
Qed.
