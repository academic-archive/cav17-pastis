Require Import pasta.Pasta.

Inductive proc: Type :=
  P_spell_out.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_spell_out_z := 1%positive.
Notation V_spell_out__tmp := 2%positive.
Notation V_spell_out_nph := 3%positive.
Notation V_spell_out_n := 4%positive.
Notation V_spell_out_phone := 5%positive.
Notation V_spell_out_word := 6%positive.
Definition Pedges_spell_out: list (edge proc) :=
  (EA 1 (AAssign V_spell_out_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_spell_out__tmp (Some (EVar V_spell_out_n))) 3)::(EA 3 (AAssign
  V_spell_out_nph (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 (AAssign
  V_spell_out__tmp (Some (EAdd (EVar V_spell_out__tmp) (ENum (-1))))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_spell_out__tmp)
  s) > (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_spell_out__tmp) s) <= (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_spell_out_nph None) 12)::(EA 12 ANone 13)::(EA 13 ANone 14)::
  (EA 14 (AAssign V_spell_out_z (Some (EAdd (ENum (1))
  (EVar V_spell_out_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_spell_out => Pedges_spell_out
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_spell_out => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_spell_out (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_spell_out_z <= 0 /\ -1 * s V_spell_out_z <= 0)%Z
   | 3 => (-1 * s V_spell_out_z <= 0 /\ 1 * s V_spell_out_z <= 0)%Z
   | 4 => (1 * s V_spell_out_z <= 0 /\ -1 * s V_spell_out_z <= 0 /\ 1 * s V_spell_out_nph <= 0 /\ -1 * s V_spell_out_nph <= 0)%Z
   | 5 => (-1 * s V_spell_out_z <= 0)%Z
   | 6 => (-1 * s V_spell_out_z <= 0)%Z
   | 7 => (-1 * s V_spell_out_z <= 0)%Z
   | 8 => (-1 * s V_spell_out_z <= 0 /\ 1 * s V_spell_out__tmp <= 0)%Z
   | 9 => (1 * s V_spell_out__tmp <= 0 /\ -1 * s V_spell_out_z <= 0)%Z
   | 10 => (-1 * s V_spell_out_z <= 0 /\ -1 * s V_spell_out__tmp + 1 <= 0)%Z
   | 11 => (-1 * s V_spell_out__tmp + 1 <= 0 /\ -1 * s V_spell_out_z <= 0)%Z
   | 12 => (-1 * s V_spell_out_z <= 0 /\ -1 * s V_spell_out__tmp + 1 <= 0)%Z
   | 13 => (-1 * s V_spell_out__tmp + 1 <= 0 /\ -1 * s V_spell_out_z <= 0)%Z
   | 14 => (-1 * s V_spell_out_z <= 0 /\ -1 * s V_spell_out__tmp + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_spell_out (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_spell_out_n) <= z)%Q
   | 2 => (s V_spell_out_z + max0(-1 + s V_spell_out_n) <= z)%Q
   | 3 => (s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | 4 => (s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | 5 => (s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | 6 => (s V_spell_out_z + max0(s V_spell_out__tmp) <= z)%Q
   | 7 => (s V_spell_out_z + max0(s V_spell_out__tmp) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_spell_out__tmp) (-1
                                                                  + s V_spell_out__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_spell_out__tmp)]
     (s V_spell_out_z + max0(s V_spell_out__tmp) <= z)%Q
   | 9 => (s V_spell_out_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_spell_out__tmp) (1)]
     (s V_spell_out_z + max0(s V_spell_out__tmp) <= z)%Q
   | 11 => ((1 # 1) + s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | 12 => ((1 # 1) + s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | 13 => ((1 # 1) + s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_spell_out_z + max0(-1 + s V_spell_out__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_spell_out =>
    [mkPA Q (fun n z s => ai_spell_out n s /\ annot0_spell_out n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_spell_out (proc_start P_spell_out) s1 (proc_end P_spell_out) s2 ->
    (s2 V_spell_out_z <= max0(-1 + s1 V_spell_out_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_spell_out.
Qed.
