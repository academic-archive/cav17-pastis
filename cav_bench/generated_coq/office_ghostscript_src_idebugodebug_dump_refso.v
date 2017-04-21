Require Import pasta.Pasta.

Inductive proc: Type :=
  P_debug_dump_refs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_debug_dump_refs_z := 1%positive.
Notation V_debug_dump_refs__tmp := 2%positive.
Notation V_debug_dump_refs_count := 3%positive.
Notation V_debug_dump_refs_from := 4%positive.
Notation V_debug_dump_refs_msg := 5%positive.
Notation V_debug_dump_refs_size := 6%positive.
Definition Pedges_debug_dump_refs: list (edge proc) :=
  (EA 1 (AAssign V_debug_dump_refs_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_debug_dump_refs__tmp (Some (EVar V_debug_dump_refs_size))) 3)::
  (EA 3 (AAssign V_debug_dump_refs_count
  (Some (EVar V_debug_dump_refs__tmp))) 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_debug_dump_refs__tmp) s) <> (eval (ENum (0))
  s))%Z)) 7)::(EA 5 (AGuard (fun s => ((eval (EVar V_debug_dump_refs__tmp)
  s) = (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 10)::(EA 7 AWeaken 8)::
  (EA 8 ANone 9)::(EA 8 ANone 10)::(EA 9 ANone 10)::(EA 10 ANone 11)::
  (EA 11 (AAssign V_debug_dump_refs_count
  (Some (EAdd (EVar V_debug_dump_refs_count) (ENum (-1))))) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_debug_dump_refs_count) s) <> (eval (ENum (0))
  s))%Z)) 16)::(EA 13 (AGuard (fun s => ((eval (EVar V_debug_dump_refs_count)
  s) = (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_debug_dump_refs_z
  (Some (EAdd (ENum (1)) (EVar V_debug_dump_refs_z)))) 11)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_debug_dump_refs => Pedges_debug_dump_refs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_debug_dump_refs => 15
     end)%positive;
  var_global := var_global
}.

Definition ai_debug_dump_refs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_debug_dump_refs_z <= 0 /\ -1 * s V_debug_dump_refs_z <= 0)%Z
   | 3 => (-1 * s V_debug_dump_refs_z <= 0 /\ 1 * s V_debug_dump_refs_z <= 0)%Z
   | 4 => (1 * s V_debug_dump_refs_z <= 0 /\ -1 * s V_debug_dump_refs_z <= 0)%Z
   | 5 => (-1 * s V_debug_dump_refs_z <= 0 /\ 1 * s V_debug_dump_refs_z <= 0)%Z
   | 6 => (1 * s V_debug_dump_refs_z <= 0 /\ -1 * s V_debug_dump_refs_z <= 0 /\ 1 * s V_debug_dump_refs__tmp <= 0 /\ -1 * s V_debug_dump_refs__tmp <= 0)%Z
   | 7 => (1 * s V_debug_dump_refs_z <= 0 /\ -1 * s V_debug_dump_refs_z <= 0)%Z
   | 8 => (-1 * s V_debug_dump_refs_z <= 0 /\ 1 * s V_debug_dump_refs_z <= 0)%Z
   | 9 => (1 * s V_debug_dump_refs_z <= 0 /\ -1 * s V_debug_dump_refs_z <= 0)%Z
   | 10 => (-1 * s V_debug_dump_refs_z <= 0 /\ 1 * s V_debug_dump_refs_z <= 0)%Z
   | 11 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | 12 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | 13 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | 14 => (-1 * s V_debug_dump_refs_z <= 0 /\ 1 * s V_debug_dump_refs_count <= 0 /\ -1 * s V_debug_dump_refs_count <= 0)%Z
   | 15 => (-1 * s V_debug_dump_refs_count <= 0 /\ 1 * s V_debug_dump_refs_count <= 0 /\ -1 * s V_debug_dump_refs_z <= 0)%Z
   | 16 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | 17 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | 18 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | 19 => (-1 * s V_debug_dump_refs_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_debug_dump_refs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_debug_dump_refs_size <= z)%Q
   | 2 => (s V_debug_dump_refs_size + s V_debug_dump_refs_z <= z)%Q
   | 3 => (s V_debug_dump_refs__tmp + s V_debug_dump_refs_z <= z)%Q
   | 4 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 5 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 6 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 7 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 8 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 9 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 10 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 11 => (s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 12 => ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 13 => ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_debug_dump_refs_count)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_debug_dump_refs_count) (0))) (F_max0_ge_0 (s V_debug_dump_refs_count))]
     ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 15 => (s V_debug_dump_refs_z <= z)%Q
   | 16 => ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 17 => ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 18 => ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | 19 => ((1 # 1) + s V_debug_dump_refs_count + s V_debug_dump_refs_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_debug_dump_refs =>
    [mkPA Q (fun n z s => ai_debug_dump_refs n s /\ annot0_debug_dump_refs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_debug_dump_refs (proc_start P_debug_dump_refs) s1 (proc_end P_debug_dump_refs) s2 ->
    (s2 V_debug_dump_refs_z <= s1 V_debug_dump_refs_size)%Q.
Proof.
  prove_bound ipa admissible_ipa P_debug_dump_refs.
Qed.
