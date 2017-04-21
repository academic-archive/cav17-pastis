Require Import pasta.Pasta.

Inductive proc: Type :=
  P_find_biggest_color_pop.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_find_biggest_color_pop_z := 1%positive.
Notation V_find_biggest_color_pop__tmp := 2%positive.
Notation V_find_biggest_color_pop_i := 3%positive.
Notation V_find_biggest_color_pop_maxc := 4%positive.
Notation V_find_biggest_color_pop_boxlist := 5%positive.
Notation V_find_biggest_color_pop_numboxes := 6%positive.
Definition Pedges_find_biggest_color_pop: list (edge proc) :=
  (EA 1 (AAssign V_find_biggest_color_pop_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_find_biggest_color_pop__tmp
  (Some (EVar V_find_biggest_color_pop_numboxes))) 3)::(EA 3 (AAssign
  V_find_biggest_color_pop_maxc (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_find_biggest_color_pop_i (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_find_biggest_color_pop_i) s) <
  (eval (EVar V_find_biggest_color_pop__tmp) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_find_biggest_color_pop_i) s) >=
  (eval (EVar V_find_biggest_color_pop__tmp) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 10 AWeaken 11)::(EA 11 ANone 12)::(EA 11 ANone 16)::
  (EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 13 ANone 16)::(EA 14 (AAssign
  V_find_biggest_color_pop_maxc None) 15)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_find_biggest_color_pop_i
  (Some (EAdd (EVar V_find_biggest_color_pop_i) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_find_biggest_color_pop_z (Some (EAdd (ENum (1))
  (EVar V_find_biggest_color_pop_z)))) 21)::(EA 21 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_find_biggest_color_pop => Pedges_find_biggest_color_pop
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_find_biggest_color_pop => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_find_biggest_color_pop (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0)%Z
   | 3 => (-1 * s V_find_biggest_color_pop_z <= 0 /\ 1 * s V_find_biggest_color_pop_z <= 0)%Z
   | 4 => (1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ 1 * s V_find_biggest_color_pop_maxc <= 0 /\ -1 * s V_find_biggest_color_pop_maxc <= 0)%Z
   | 5 => (-1 * s V_find_biggest_color_pop_maxc <= 0 /\ 1 * s V_find_biggest_color_pop_maxc <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ 1 * s V_find_biggest_color_pop_z <= 0 /\ 1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 6 => (-1 * s V_find_biggest_color_pop_i <= 0 /\ 1 * s V_find_biggest_color_pop_i <= 0 /\ 1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ 1 * s V_find_biggest_color_pop_maxc <= 0 /\ -1 * s V_find_biggest_color_pop_maxc <= 0)%Z
   | 7 => (-1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 8 => (-1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ 1 * s V_find_biggest_color_pop__tmp+ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 9 => (1 * s V_find_biggest_color_pop__tmp+ -1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 10 => (-1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0)%Z
   | 11 => (-1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 12 => (-1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0)%Z
   | 13 => (-1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 14 => (-1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0)%Z
   | 15 => (-1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 16 => (-1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0)%Z
   | 17 => (-1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i + 1 <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop_i <= 0)%Z
   | 18 => (-1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_i + 1 <= 0)%Z
   | 19 => (-1 * s V_find_biggest_color_pop_i + 1 <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z <= 0)%Z
   | 20 => (-1 * s V_find_biggest_color_pop_z <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_i + 1 <= 0)%Z
   | 21 => (-1 * s V_find_biggest_color_pop_i + 1 <= 0 /\ -1 * s V_find_biggest_color_pop__tmp+ 1 * s V_find_biggest_color_pop_i <= 0 /\ -1 * s V_find_biggest_color_pop_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_find_biggest_color_pop (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_find_biggest_color_pop_numboxes) <= z)%Q
   | 2 => (s V_find_biggest_color_pop_z
           + max0(s V_find_biggest_color_pop_numboxes) <= z)%Q
   | 3 => (s V_find_biggest_color_pop_z
           + max0(s V_find_biggest_color_pop__tmp) <= z)%Q
   | 4 => (s V_find_biggest_color_pop_z
           + max0(s V_find_biggest_color_pop__tmp) <= z)%Q
   | 5 => (s V_find_biggest_color_pop_z
           + max0(s V_find_biggest_color_pop__tmp
                  - s V_find_biggest_color_pop_i) <= z)%Q
   | 6 => (s V_find_biggest_color_pop_z
           + max0(s V_find_biggest_color_pop__tmp
                  - s V_find_biggest_color_pop_i) <= z)%Q
   | 7 => (s V_find_biggest_color_pop_z
           + max0(s V_find_biggest_color_pop__tmp
                  - s V_find_biggest_color_pop_i) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_find_biggest_color_pop__tmp
                                             - s V_find_biggest_color_pop_i) (-1
                                                                    + s V_find_biggest_color_pop__tmp
                                                                    - s V_find_biggest_color_pop_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_find_biggest_color_pop__tmp
                            - s V_find_biggest_color_pop_i)]
     (s V_find_biggest_color_pop_z
      + max0(s V_find_biggest_color_pop__tmp - s V_find_biggest_color_pop_i) <= z)%Q
   | 9 => (s V_find_biggest_color_pop_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_find_biggest_color_pop__tmp
                                       - s V_find_biggest_color_pop_i) (1)]
     (s V_find_biggest_color_pop_z
      + max0(s V_find_biggest_color_pop__tmp - s V_find_biggest_color_pop_i) <= z)%Q
   | 11 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 12 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 13 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 14 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 15 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 16 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 17 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(-1 + s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 18 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 19 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 20 => ((1 # 1) + s V_find_biggest_color_pop_z
            + max0(s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | 21 => (s V_find_biggest_color_pop_z
            + max0(s V_find_biggest_color_pop__tmp
                   - s V_find_biggest_color_pop_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_find_biggest_color_pop =>
    [mkPA Q (fun n z s => ai_find_biggest_color_pop n s /\ annot0_find_biggest_color_pop n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_find_biggest_color_pop (proc_start P_find_biggest_color_pop) s1 (proc_end P_find_biggest_color_pop) s2 ->
    (s2 V_find_biggest_color_pop_z <= max0(s1 V_find_biggest_color_pop_numboxes))%Q.
Proof.
  prove_bound ipa admissible_ipa P_find_biggest_color_pop.
Qed.
