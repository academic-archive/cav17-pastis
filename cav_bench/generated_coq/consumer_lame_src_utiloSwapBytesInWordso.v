Require Import pasta.Pasta.

Inductive proc: Type :=
  P_SwapBytesInWords.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_SwapBytesInWords_z := 1%positive.
Notation V_SwapBytesInWords__tmp := 2%positive.
Notation V_SwapBytesInWords_i := 3%positive.
Notation V_SwapBytesInWords_loc := 4%positive.
Notation V_SwapBytesInWords_words := 5%positive.
Definition Pedges_SwapBytesInWords: list (edge proc) :=
  (EA 1 (AAssign V_SwapBytesInWords_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_SwapBytesInWords__tmp (Some (EVar V_SwapBytesInWords_words))) 3)::
  (EA 3 (AAssign V_SwapBytesInWords_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_SwapBytesInWords_i) s) <
  (eval (EVar V_SwapBytesInWords__tmp) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_SwapBytesInWords_i) s) >=
  (eval (EVar V_SwapBytesInWords__tmp) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 11 (AAssign V_SwapBytesInWords_i
  (Some (EAdd (EVar V_SwapBytesInWords_i) (ENum (1))))) 12)::
  (EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign V_SwapBytesInWords_z
  (Some (EAdd (ENum (1)) (EVar V_SwapBytesInWords_z)))) 15)::
  (EA 15 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_SwapBytesInWords => Pedges_SwapBytesInWords
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_SwapBytesInWords => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_SwapBytesInWords (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0)%Z
   | 3 => (-1 * s V_SwapBytesInWords_z <= 0 /\ 1 * s V_SwapBytesInWords_z <= 0)%Z
   | 4 => (1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ 1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_i <= 0)%Z
   | 5 => (-1 * s V_SwapBytesInWords_i <= 0 /\ 1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ 1 * s V_SwapBytesInWords_z <= 0)%Z
   | 6 => (-1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_i <= 0)%Z
   | 7 => (-1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ 1 * s V_SwapBytesInWords__tmp+ -1 * s V_SwapBytesInWords_i <= 0)%Z
   | 8 => (1 * s V_SwapBytesInWords__tmp+ -1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_i <= 0)%Z
   | 9 => (-1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i + 1 <= 0)%Z
   | 10 => (-1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i + 1 <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_i <= 0)%Z
   | 11 => (-1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i + 1 <= 0)%Z
   | 12 => (-1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_i + 1 <= 0 /\ -1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i <= 0)%Z
   | 13 => (-1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_i + 1 <= 0 /\ -1 * s V_SwapBytesInWords_z <= 0)%Z
   | 14 => (-1 * s V_SwapBytesInWords_z <= 0 /\ -1 * s V_SwapBytesInWords_i + 1 <= 0 /\ -1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i <= 0)%Z
   | 15 => (-1 * s V_SwapBytesInWords__tmp+ 1 * s V_SwapBytesInWords_i <= 0 /\ -1 * s V_SwapBytesInWords_i + 1 <= 0 /\ -1 * s V_SwapBytesInWords_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_SwapBytesInWords (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_SwapBytesInWords_words) <= z)%Q
   | 2 => (s V_SwapBytesInWords_z + max0(s V_SwapBytesInWords_words) <= z)%Q
   | 3 => (s V_SwapBytesInWords_z + max0(s V_SwapBytesInWords__tmp) <= z)%Q
   | 4 => (s V_SwapBytesInWords_z
           + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 5 => (s V_SwapBytesInWords_z
           + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 6 => (s V_SwapBytesInWords_z
           + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_SwapBytesInWords__tmp
                                             - s V_SwapBytesInWords_i) (-1
                                                                    + s V_SwapBytesInWords__tmp
                                                                    - s V_SwapBytesInWords_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_SwapBytesInWords__tmp
                                                 - s V_SwapBytesInWords_i)) (F_check_ge (0) (0))]
     (s V_SwapBytesInWords_z
      + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 8 => (s V_SwapBytesInWords_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_SwapBytesInWords__tmp
                                       - s V_SwapBytesInWords_i) (1)]
     (s V_SwapBytesInWords_z
      + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 10 => ((1 # 1) + s V_SwapBytesInWords_z
            + max0(-1 + s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 11 => ((1 # 1) + s V_SwapBytesInWords_z
            + max0(-1 + s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 12 => ((1 # 1) + s V_SwapBytesInWords_z
            + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 13 => ((1 # 1) + s V_SwapBytesInWords_z
            + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 14 => ((1 # 1) + s V_SwapBytesInWords_z
            + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | 15 => (s V_SwapBytesInWords_z
            + max0(s V_SwapBytesInWords__tmp - s V_SwapBytesInWords_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_SwapBytesInWords =>
    [mkPA Q (fun n z s => ai_SwapBytesInWords n s /\ annot0_SwapBytesInWords n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_SwapBytesInWords (proc_start P_SwapBytesInWords) s1 (proc_end P_SwapBytesInWords) s2 ->
    (s2 V_SwapBytesInWords_z <= max0(s1 V_SwapBytesInWords_words))%Q.
Proof.
  prove_bound ipa admissible_ipa P_SwapBytesInWords.
Qed.
