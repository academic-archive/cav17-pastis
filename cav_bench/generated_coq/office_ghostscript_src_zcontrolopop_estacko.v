Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pop_estack.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pop_estack_z := 1%positive.
Notation V_pop_estack__tmp := 2%positive.
Notation V_pop_estack_idx := 3%positive.
Notation V_pop_estack_popped := 4%positive.
Notation V_pop_estack_count := 5%positive.
Definition Pedges_pop_estack: list (edge proc) :=
  (EA 1 (AAssign V_pop_estack_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_pop_estack_idx) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_pop_estack__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_pop_estack__tmp (Some (EVar V_pop_estack_count))) 6)::(EA 6 (AAssign
  V_pop_estack_idx (Some (ENum (0)))) 7)::(EA 7 (AAssign V_pop_estack_popped
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_pop_estack_idx) s) <
  (eval (EVar V_pop_estack__tmp) s))%Z)) 13)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_pop_estack_idx) s) >=
  (eval (EVar V_pop_estack__tmp) s))%Z)) 11)::(EA 11 AWeaken 12)::
  (EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 14 ANone 17)::(EA 15 (AAssign
  V_pop_estack_popped (Some (EAdd (EVar V_pop_estack_idx) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_pop_estack_idx
  (Some (EAdd (EVar V_pop_estack_idx) (ENum (1))))) 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_pop_estack_z (Some (EAdd (ENum (1))
  (EVar V_pop_estack_z)))) 22)::(EA 22 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pop_estack => Pedges_pop_estack
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pop_estack => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_pop_estack (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_z <= 0)%Z
   | 3 => (-1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_idx <= 0)%Z
   | 4 => (-1 * s V_pop_estack_idx <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack__tmp <= 0)%Z
   | 5 => (-1 * s V_pop_estack__tmp <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_idx <= 0)%Z
   | 6 => (-1 * s V_pop_estack_idx <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_z <= 0)%Z
   | 7 => (-1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_idx <= 0)%Z
   | 8 => (-1 * s V_pop_estack_idx <= 0 /\ 1 * s V_pop_estack_idx <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_popped <= 0)%Z
   | 9 => (-1 * s V_pop_estack_popped <= 0 /\ 1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_z <= 0 /\ 1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_idx <= 0)%Z
   | 10 => (-1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_popped <= 0)%Z
   | 11 => (-1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ 1 * s V_pop_estack__tmp+ -1 * s V_pop_estack_idx <= 0)%Z
   | 12 => (1 * s V_pop_estack__tmp+ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_popped <= 0)%Z
   | 13 => (-1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx + 1 <= 0)%Z
   | 14 => (-1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx + 1 <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_popped <= 0)%Z
   | 15 => (-1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx + 1 <= 0)%Z
   | 16 => (-1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx + 1 <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_popped + 1 <= 0)%Z
   | 17 => (-1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx + 1 <= 0)%Z
   | 18 => (-1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx + 1 <= 0 /\ -1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_popped <= 0)%Z
   | 19 => (-1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_idx + 1 <= 0)%Z
   | 20 => (-1 * s V_pop_estack_idx + 1 <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack_popped <= 0)%Z
   | 21 => (-1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_idx + 1 <= 0)%Z
   | 22 => (-1 * s V_pop_estack_idx + 1 <= 0 /\ -1 * s V_pop_estack__tmp+ 1 * s V_pop_estack_idx <= 0 /\ -1 * s V_pop_estack_popped <= 0 /\ -1 * s V_pop_estack_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pop_estack (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_pop_estack_count) <= z)%Q
   | 2 => (s V_pop_estack_z + max0(s V_pop_estack_count) <= z)%Q
   | 3 => (s V_pop_estack_z + max0(s V_pop_estack_count) <= z)%Q
   | 4 => (s V_pop_estack_z + max0(s V_pop_estack_count) <= z)%Q
   | 5 => (s V_pop_estack_z + max0(s V_pop_estack_count) <= z)%Q
   | 6 => (s V_pop_estack_z + max0(s V_pop_estack__tmp) <= z)%Q
   | 7 => (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 8 => (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 9 => (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 10 => (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pop_estack__tmp
                                             - s V_pop_estack_idx) (-1
                                                                    + 
                                                                    s V_pop_estack__tmp
                                                                    - 
                                                                    s V_pop_estack_idx));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pop_estack__tmp - s V_pop_estack_idx)]
     (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 12 => (s V_pop_estack_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pop_estack__tmp
                                       - s V_pop_estack_idx) (1)]
     (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 14 => ((1 # 1) + s V_pop_estack_z
            + max0(-1 + s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 15 => ((1 # 1) + s V_pop_estack_z
            + max0(-1 + s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 16 => ((1 # 1) + s V_pop_estack_z
            + max0(-1 + s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 17 => ((1 # 1) + s V_pop_estack_z
            + max0(-1 + s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 18 => ((1 # 1) + s V_pop_estack_z
            + max0(-1 + s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 19 => ((1 # 1) + s V_pop_estack_z
            + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 20 => ((1 # 1) + s V_pop_estack_z
            + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 21 => ((1 # 1) + s V_pop_estack_z
            + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | 22 => (s V_pop_estack_z + max0(s V_pop_estack__tmp - s V_pop_estack_idx) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pop_estack =>
    [mkPA Q (fun n z s => ai_pop_estack n s /\ annot0_pop_estack n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pop_estack (proc_start P_pop_estack) s1 (proc_end P_pop_estack) s2 ->
    (s2 V_pop_estack_z <= max0(s1 V_pop_estack_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pop_estack.
Qed.
